0.3
---

* Add the ability to display multiple responses, with some accompanying `Text` to describe the context in which we get the corresponding JSON.
* Expose the `headers` lens
* Represent an endpoint's path as `[String]` (previously `String`), fixing a corner case where the leading `/` would be missing.
