# devremote-mode


``` lisp
(use-package devremote
  :load-path (lambda () (expand-file-name "~/xxxx/devremote-mode"))
  :bind (("<f12>" . 'devremote-transfer-current)
  	 ("<f11>" . 'devremote-transfer-project)
  	 ("M-<f12>" . 'devremote-compilation-project))
  )
```
