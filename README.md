# devremote-mode

## how install

1. If not already enabled, enable `quelpa-use-package`.
``` lisp
(use-package quelpa-use-package)
```

2. install `devremote` from github
``` lisp
(use-package devremote
  :ensure t
  :quelpa (devremote :fetcher github :repo "snyh/devremote")
  :bind (("<f12>" . 'devremote-transfer-current)
  	 ("<f11>" . 'devremote-transfer-project)
  	 ("M-<f12>" . 'devremote-compilation-project))
  )
```

## Usage
0. Log in to the server with ssh and configure `~/.ssh/config` correctly.
1. Open any project file with emacs
2. Invoke `M-x` `devremote-mode`
3. Invoke `M-x` `devremote-create-project`
