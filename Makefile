TEST_FILES := $(wildcard tests/**/*_test.nix)

test:
	./scripts/run-tests $(TEST_FILES)
.PHONY: tests


# jq --raw-output "if length != 0 then . else \"$f: success\" end"; done
