FILES  = src/Main.hs src/Spec.hs src/schema.sql

src/$(FILES): Announcement.anansi
	anansi tangle -o "src" Announcement.anansi

announcement.md: Announcement.anansi
	anansi weave -o "announcement.tmp" Announcement.anansi
	cat announcement.tmp | tr -d '«' | tr -d '»' > announcement.md
	rm announcement.tmp

.stack-work/bin/posts: $(FILES) stack.yaml posts.cabal
	stack build

announcement.html: announcement.md
	pandoc announcement.md -t html > announcement.html

run: .stack-work/bin/posts
	stack exec posts

test: .stack-work/bin/posts
	stack test

post: announcement.html

.PHONY: post run test
