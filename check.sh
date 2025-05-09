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

readonly EMACS="emacs -Q --batch"
readonly ELISP_FILES="\"mcp.el\" \"mcp-test.el\" \"mcp-test-bytecode-handler.el\""

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
	echo "Running elisp-autofmt on Elisp files..."
	$EMACS --eval "(let ((pkg-dirs (list (locate-user-emacs-file \"elpa/elisp-autofmt-20250421.1112\")
	                                      (expand-file-name \".\"))))
	                     (dolist (dir pkg-dirs)
	                       (add-to-list 'load-path dir))
	                     (require 'elisp-autofmt)
	                     (dolist (file '($ELISP_FILES))
	                       (message \"Formatting %s...\" file)
	                       (find-file file)
	                       (elisp-autofmt-buffer-to-file)
	                       (message \"Formatted %s\" file)))" || {
		echo "elisp-autofmt failed"
		ERRORS=$((ERRORS + 1))
	}
else
	echo "Skipping indentation due to syntax errors"
fi

echo "Running elisp-lint on Emacs Lisp files..."
$EMACS --eval "(let ((pkg-dirs (list (locate-user-emacs-file \"elpa/elisp-lint-20220419.252\")
                                      (locate-user-emacs-file \"elpa/package-lint-0.26\")
                                      (locate-user-emacs-file \"elpa/dash-20250312.1307\")
                                      (expand-file-name \".\"))))
                     (dolist (dir pkg-dirs)
                       (add-to-list 'load-path dir))
                     (require 'elisp-lint)
                     (dolist (file (list $ELISP_FILES))
                       (message \"Linting %s...\" file)
                       (elisp-lint-file file)))" || {
	echo "elisp-lint failed"
	ERRORS=$((ERRORS + 1))
}

echo "Running all tests..."
$EMACS -l mcp.el -l mcp-test.el --eval '(ert-run-tests-batch-and-exit)' || {
	echo "ERT tests failed"
	ERRORS=$((ERRORS + 1))
}

echo "Checking Markdown files..."
mdl ./*.md || {
	echo "mdl check failed"
	ERRORS=$((ERRORS + 1))
}

echo "Checking README.org..."
$EMACS --eval "(require 'org)" --eval "(require 'org-lint)" \
	--eval "(with-temp-buffer (insert-file-contents \"README.org\") \
             (org-mode) (let ((results (org-lint))) \
             (if results (progn (message \"Found issues: %S\" results) (exit 1)) \
             (message \"No issues found\"))))" || {
	echo "README.org check failed"
	ERRORS=$((ERRORS + 1))
}

echo "Checking TODO.org..."
$EMACS --eval "(require 'org)" --eval "(require 'org-lint)" \
	--eval "(with-temp-buffer (insert-file-contents \"TODO.org\") \
             (org-mode) (let ((results (org-lint))) \
             (if results (progn (message \"Found issues: %S\" results) (exit 1)) \
             (message \"No issues found\"))))" || {
	echo "TODO.org check failed"
	ERRORS=$((ERRORS + 1))
}

echo "Checking for code duplication..."
jscpd -r consoleFull -t 0 . || {
	echo "jscpd check failed"
	ERRORS=$((ERRORS + 1))
}

echo "Checking GitHub workflows..."
actionlint .github/workflows/*.yml || {
	echo "actionlint check failed"
	ERRORS=$((ERRORS + 1))
}

echo "Checking YAML formatting..."
prettier --check .github/workflows/*.yml || {
	echo "prettier check failed"
	ERRORS=$((ERRORS + 1))
}

echo "Checking Markdown formatting..."
prettier --check ./*.md || {
	echo "prettier check for Markdown failed"
	ERRORS=$((ERRORS + 1))
}

echo "Checking terminology..."
textlint --rule terminology ./*.md || {
	echo "textlint check failed"
	ERRORS=$((ERRORS + 1))
}

echo "Running shellcheck..."
shellcheck check.sh emacs-mcp-stdio.sh emacs-mcp-stdio-test.sh || {
	echo "shellcheck check failed"
	ERRORS=$((ERRORS + 1))
}

echo "Running stdio adapter tests..."
./emacs-mcp-stdio-test.sh || {
	echo "stdio adapter tests failed"
	ERRORS=$((ERRORS + 1))
}

# Final result
if [ $ERRORS -eq 0 ]; then
	echo "Running shfmt to format all shell scripts..."
	shfmt -w ./*.sh
	echo "OK to proceed"
else
	echo "$ERRORS check(s) failed"
	exit 1
fi
