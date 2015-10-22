all:
	echo Nothing to do...

ctags:
	ctags --options=ctags.rust --languages=Rust src/*.rs src/*/*.rs

docs:
	cargo doc
	in-dir ./target/doc fix-perms
	rscp ./target/doc/* gopher:~/www/burntsushi.net/rustdoc/

push:
	git push origin master
	git push github master
