github-post-receive
===================

`github-post-receive <port-number> <config-file>`

sample of config file
```js
[{
    "name": "foo",
    "scripts": [
        "/path/to/foo.sh",
        "ls -la"
    ]
}, {
    "name": "bar",
    "scripts": [
        "/path/to/bar.sh"
    ]
}]
```
