FILES  = src/Main.hs src/Spec.hs src/schema.sql

src/$(FILES): Announcement.anansi
	anansi tangle -o "src" Announcement.anansi

announcement.md: Announcement.anansi
	anansi weave -o "announcement.md" Announcement.anansi

.stack-work/bin/posts: $(FILES) stack.yaml posts.cabal
	stack build



run: .stack-work/bin/posts
	stack exec posts

test: .stack-work/bin/posts
	stack test

post: announcement.md

.PHONY: post run test
