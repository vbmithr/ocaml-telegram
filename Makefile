all:
	jbuilder build @install @runtest test/test_bot.exe

.PHONY: clean
clean:
	rm -rf _build
