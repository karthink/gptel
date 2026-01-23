.PHONY: test
test:
	emacs -Q -L .. -L . \
	$$(find .. -mindepth 1 -maxdepth 1 \( -name '.*.el' -prune -o -name '*.el' -type f -printf ' -l %p' \)) \
	$$(find .  -mindepth 1 -maxdepth 1 \( -name '.*.el' -prune -o -name '*.el' -type f -printf ' -l %p' \)) \
	-l ert --batch -f ert-run-tests-batch-and-exit
