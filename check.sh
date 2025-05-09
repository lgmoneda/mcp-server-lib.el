#!/bin/bash
# check.sh - Run all quality checks, with focus on providing guardrails for LLM
# coding agents.
#
# Continue on errors but track them

# Partial ordering between the linters and autoformatters:
# - Start with the Elisp syntax check (via byte compilation)
#   - Catches major issues like unbalanced parentheses
#   - LLMs tend to generate syntax errors, so check these first
#
# - If no syntax errors:
#   - Run elisp-autofmt to format Elisp files automatically
#     (Prevents incorrect indentation when syntax is broken)
#   - Then run elisp-lint for additional style checks
#     (May catch minor issues elisp-autofmt couldn't fix, like long docstrings)

set -eu -o pipefail

readonly ELISP_FILES="\"mcp.el\" \"mcp-test.el\" \"mcp-test-bytecode-handler.el\""

readonly EMACS="emacs -Q --batch"

# Elisp packages in ELPA
readonly ELISP_AUTOFMT="elisp-autofmt-20250421.1112"
readonly ELISP_LINT="elisp-lint-20220419.252"
readonly PACKAGE_LINT="package-lint-0.26"
readonly DASH="dash-20250312.1307"

ERRORS=0

echo -n "Checking Elisp syntax... "
if $EMACS --eval "(setq byte-compile-warnings nil)" \
	--eval "(add-to-list 'load-path \".\")" \
	--eval "(dolist (file '($ELISP_FILES))
        (princ (format \"%s \" file))
        (unless (byte-compile-file file)
          (kill-emacs 1)))"; then
	echo "OK!"
else
	echo "Elisp syntax check failed!"
	ERRORS=$((ERRORS + 1))
fi

# Only run indentation if there are no errors so far
if [ $ERRORS -eq 0 ]; then
	echo -n "Running elisp-autofmt... "
	if $EMACS --eval "(add-to-list 'load-path (locate-user-emacs-file \"elpa/$ELISP_AUTOFMT\"))" \
		--eval "(add-to-list 'load-path (expand-file-name \".\"))" \
		--eval "(require 'elisp-autofmt)" \
		--eval "(dolist (file '($ELISP_FILES))
                   (princ (format \"%s \" file))
	           (find-file file)
	           (elisp-autofmt-buffer-to-file))"; then
		echo "OK!"
	else
		echo "elisp-autofmt failed!"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "Skipping indentation due to syntax errors"
fi

# Only run elisp-lint if there are no errors so far
if [ $ERRORS -eq 0 ]; then
	echo -n "Running elisp-lint... "
	if $EMACS --eval "(let ((pkg-dirs (list (locate-user-emacs-file \"elpa/$ELISP_LINT\")
	                                      (locate-user-emacs-file \"elpa/$PACKAGE_LINT\")
	                                      (locate-user-emacs-file \"elpa/$DASH\")
	                                      (expand-file-name \".\"))))
	                     (dolist (dir pkg-dirs)
	                       (add-to-list 'load-path dir))
	                     (require 'elisp-lint)
	                     (dolist (file (list $ELISP_FILES))
                               (princ (format \"%s \" file))
	                       (elisp-lint-file file)))"; then
		echo "OK!"
	else
		echo "elisp-lint failed"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "Skipping elisp-lint due to previous errors"
fi

echo -n "Running all tests... "
if $EMACS -l mcp.el -l mcp-test.el --eval '(ert-run-tests-batch-and-exit)'; then
	echo "OK!"
else
	echo "ERT tests failed"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking Markdown files... "
if mdl ./*.md; then
	echo "OK!"
else
	echo "mdl check failed"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking README.org... "
if $EMACS --eval "(require 'org)" --eval "(require 'org-lint)" \
	--eval "(with-temp-buffer (insert-file-contents \"README.org\") \
             (org-mode) (let ((results (org-lint))) \
             (if results (progn (message \"Found issues: %S\" results) (exit 1)) \
             (message \"No issues found\"))))"; then
	echo "OK!"
else
	echo "README.org check failed"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking TODO.org... "
if $EMACS --eval "(require 'org)" --eval "(require 'org-lint)" \
	--eval "(with-temp-buffer (insert-file-contents \"TODO.org\") \
             (org-mode) (let ((results (org-lint))) \
             (if results (progn (message \"Found issues: %S\" results) (exit 1)) \
             (message \"No issues found\"))))"; then
	echo "OK!"
else
	echo "TODO.org check failed"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking for code duplication... "
if jscpd -r consoleFull -t 0 .; then
	echo "OK!"
else
	echo "jscpd check failed"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking GitHub workflows... "
if actionlint .github/workflows/*.yml; then
	echo "OK!"
else
	echo "actionlint check failed"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking YAML formatting... "
if prettier --check .github/workflows/*.yml; then
	echo "OK!"
else
	echo "prettier check failed"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking Markdown formatting... "
if prettier --check ./*.md; then
	echo "OK!"
else
	echo "prettier check for Markdown failed"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking terminology... "
if textlint --rule terminology ./*.md; then
	echo "OK!"
else
	echo "textlint check failed"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Running shellcheck... "
if shellcheck check.sh emacs-mcp-stdio.sh emacs-mcp-stdio-test.sh; then
	echo "OK!"
else
	echo "shellcheck check failed"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Running stdio adapter tests... "
if ./emacs-mcp-stdio-test.sh; then
	echo "OK!"
else
	echo "stdio adapter tests failed"
	ERRORS=$((ERRORS + 1))
fi

# Final result
if [ $ERRORS -eq 0 ]; then
	echo -n "Running shfmt to format all shell scripts... "
	if shfmt -w ./*.sh; then
		echo "OK!"
		echo "All checks passed successfully!"
	else
		echo "shfmt failed!"
		ERRORS=$((ERRORS + 1))
		echo "$ERRORS check(s) failed"
		exit 1
	fi
else
	echo "$ERRORS check(s) failed"
	exit 1
fi
