# devremote-mode

## how install

1. If not already enabled, enable `quelpa-use-package`.
``` lisp
(use-package quelpa-use-package)
```

2. install `devremote` from github
``` lisp
(use-package devremote
  :hook (find-file . devremote-detect)
  :quelpa (devremote :fetcher github :repo "snyh/devremote")
  :bind (:map devremote-mode-map
	 ("<f12>" . 'devremote-transfer-current)
  	 ("<f11>" . 'devremote-transfer-project)
  	 ("M-<f12>" . 'devremote-compilation-project))
  )
```

## Usage
0. Log in to the server with ssh and configure `~/.ssh/config` correctly.
1. Open any project file with emacs
3. Invoke `M-x` `devremote-create-project`
