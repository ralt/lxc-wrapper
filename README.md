# lxc-wrapper

My personal LXC wrapper.

## Motivation

I use LXC in a very opinionated way, and has some manual maintenance
to do every time I do something with LXCs. So I created this tool to
automate what I do with them.

## API

### `create`

```lisp
(defun create (name &key base template)
  "Creates an LXC"
```

Creates an LXC. If a base LXC is provided, then it makes a clone of
it. If a template is provided, then it creates a new LXC based on this
template.

### `start`

```lisp
(defun start (name)
  "Starts an LXC"
```

Starts an LXC. The argument can be a string or a symbol.

### `stop`

```lisp
(defun stop (name)
  "Stops an LXC"
```

Stops an LXC. The argument can be a string or a symbol.

### `ls`

```lisp
(defun ls ()
  "Lists all the LXC"
```

Returns the fancy output of the list of LXCs.

## License

MIT License.
