#!/bin/bash
# check.sh - Run all quality checks for mcp.el
# Continue on errors but track them

# Track if any errors occurred
ERRORS=0

echo "Running elisp-lint on Emacs Lisp files..."
emacs -batch --eval "(let ((pkg-dirs (list (locate-user-emacs-file \"elpa/elisp-lint-20220419.252\")
                                      (locate-user-emacs-file \"elpa/package-lint-0.26\")
                                      (locate-user-emacs-file \"elpa/dash-20250312.1307\")
                                      (expand-file-name \".\"))))
                     (dolist (dir pkg-dirs)
                       (add-to-list 'load-path dir))
                     (require 'elisp-lint)
                     (elisp-lint-file \"mcp.el\")
                     (elisp-lint-file \"mcp-test.el\"))" || {
	echo "elisp-lint failed"
	ERRORS=$((ERRORS + 1))
}

echo "Running all tests..."
emacs -Q -batch -l mcp.el -l mcp-test.el --eval '(ert-run-tests-batch-and-exit)' || {
	echo "ERT tests failed"
	ERRORS=$((ERRORS + 1))
}

echo "Checking Markdown files..."
mdl ./*.md || {
	echo "mdl check failed"
	ERRORS=$((ERRORS + 1))
}

echo "Checking README.org..."
emacs -Q --batch --eval "(require 'org)" --eval "(require 'org-lint)" \
	--eval "(with-temp-buffer (insert-file-contents \"README.org\") \
             (org-mode) (let ((results (org-lint))) \
             (if results (progn (message \"Found issues: %S\" results) (exit 1)) \
             (message \"No issues found\"))))" || {
	echo "README.org check failed"
	ERRORS=$((ERRORS + 1))
}

echo "Checking TODO.org..."
emacs -Q --batch --eval "(require 'org)" --eval "(require 'org-lint)" \
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

echo "Checking terminology..."
textlint --rule terminology ./*.md || {
	echo "textlint check failed"
	ERRORS=$((ERRORS + 1))
}

echo "Running shellcheck..."
shellcheck check.sh || {
	echo "shellcheck check failed"
	ERRORS=$((ERRORS + 1))
}

# Final result
if [ $ERRORS -eq 0 ]; then
	echo "Running shfmt to format shell script..."
	shfmt -w check.sh
	echo "OK to proceed"
else
	echo "$ERRORS check(s) failed"
	exit 1
fi
