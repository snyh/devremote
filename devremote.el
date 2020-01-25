;;; devremote.el --- Make transfering code to remote server
;;; -*- coding: utf-8; lexical-binding: t; -*-
;;;      more easy for temporary development and debugging.
;;; Commentary:
;;    automatically transfer to remote server.
;;

(require 'subr-x)
(require 'easymenu)
(require 'projectile)
(require 'pcmpl-unix)
(require 'helm)

;;;; code:
(defvar devremote-mode-lighter " DR")

(defvar devremote-default-server "/ssh:sw-sh")

(defun error-not-in-project (file)
  "FILE is not in project."
  (user-error "The file %s isn't in any project"
              file))

(cl-defstruct -pinfo
  server
  local-root-dir
  ignore-dirs
  remote-root-dir
  build-cmd)

(setq-default devremote-project-infos nil)

(defcustom devremote-known-projects-file
  (expand-file-name "devremote.projects" user-emacs-directory)
  "The name of devremote's known projects file."
  :group 'devremote
  :type 'string)

(defun devremote-serialize (data filename)
  "Serialize the DATA configure to FILENAME."
  (with-temp-file filename
    (insert (prin1-to-string data))))

(defun devremote-unserialize (filename)
  "Read data serialized by `devremote-serialize` from FILENAME."
  (with-demoted-errors
      "Error during devremote-unserialize: %S\n"
    (when (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (read (buffer-string)))
      )))

(defun devremote-save-known-projects ()
  (devremote-serialize devremote-project-infos devremote-known-projects-file))

(defun devremote-load-known-projects ()
  (setq devremote-project-infos
        (devremote-unserialize devremote-known-projects-file)))

(defun -in-dir(f dir) (string-prefix-p dir f))
(defun -not-in-dir (f dir) (not (-in-dir f dir)))

(defun --expand-ignore-dirs (root dirs)
  (mapcar (lambda (elt) (expand-file-name (concat root "/" elt)))
          dirs))

(defun --pinfo-contain-file (pinfo file)
  "Check whether the PINFO contain FILE.

FILE must in local root directory and must not in any of ignore directories."
  (if file
      (let ((_local-root (expand-file-name (-pinfo-local-root-dir pinfo)))
	    (_ignore-dirs (-pinfo-ignore-dirs pinfo))
	    (_file (expand-file-name file)))
	(seq-reduce (lambda (acc elt) (and acc (-not-in-dir _file elt)))
		    (--expand-ignore-dirs _local-root _ignore-dirs)
		    (-in-dir _file _local-root)))))

(defun devremote-query-pinfo (file)
  (interactive "bBuffer")
  (-first (lambda (pinfo) (--pinfo-contain-file pinfo file))
          devremote-project-infos))

(defun in-any-project-p (file) (devremote-query-pinfo file))

(defun -pinfo-base-name (name pinfo)
  (file-relative-name name (-pinfo-local-root-dir pinfo)))

(defun -pinfo-local-file (name pinfo)
  (expand-file-name
   (concat
    (-pinfo-local-root-dir pinfo)
    "/"
    (-pinfo-base-name name pinfo))))

(defun -pinfo-remote-file (name pinfo)
  (expand-file-name
   (concat
    (-pinfo-server pinfo)
    ":"
    (-pinfo-remote-root-dir pinfo)
    "/"
    (-pinfo-base-name name pinfo))))

(defun -copy-to-remote (long-name)
  (let ((pinfo (devremote-query-pinfo long-name)))
    (if pinfo
        (copy-file
         (-pinfo-local-file long-name pinfo)
         (-pinfo-remote-file long-name pinfo)
         t)
      (error-not-in-project long-name)
      )))

(defun devremote-transfer-project ()
  (interactive)
  (let* ((pinfo (devremote-query-pinfo buffer-file-name)))
    (unless pinfo (error-not-in-project buffer-file-name))
    (compilation-start (-build-rsync-cmd pinfo))))


(defun --devremote-select-ssh-server()
  (let ((ssh-helm-source
	 `(
	   (name . "选择服务器")
	   (candidates . ,(pcmpl-ssh-config-hosts))
	   (action . (lambda (s) (format "/ssh:%s" s)))
	   )
	 ))
    (helm
     :sources
     (list
      ssh-helm-source
      (helm-build-dummy-source "手动指定"
	:action  (lambda (s) (format "/ssh:%s" s)))
      )
     :prompt "Select Remote Server: ")
    )
  )

(defun devremote-switch-server ()
  "Request switch server to host."
  (interactive)
  (let* ((pinfo (devremote-query-pinfo buffer-file-name)))
    (unless pinfo (error-not-in-project buffer-file-name))
    (setf (-pinfo-server pinfo) (--devremote-select-ssh-server))
    (--devremote-update-lighter)
    ))

(defun devremote-compilation-project ()
  (interactive)
  (let ((pinfo (devremote-query-pinfo buffer-file-name)))
    (unless pinfo (error-not-in-project buffer-file-name))
    (let* ((remote-root-dir (-pinfo-remote-root-dir pinfo))
           (local-root-dir (expand-file-name (-pinfo-local-root-dir pinfo)))
           (remote-server (string-remove-prefix "/ssh:" (-pinfo-server pinfo)))
           (remote-cmd (or (-pinfo-build-cmd pinfo) "make"))
           (compile-command (format "ssh %s \"cd %s && %s\""
                                    remote-server
                                    remote-root-dir
                                    remote-cmd
                                    )))
      (compilation-start compile-command))))


(defun -build-rsync-cmd (pinfo)
  (let* ((local-dir (-pinfo-local-root-dir pinfo))
         (server (string-remove-prefix "/ssh:" (-pinfo-server pinfo)))
         (remote-dir (-pinfo-remote-root-dir pinfo))
         (ignores (-reduce-from
                   (lambda (acc elt)
                     (format "%s --exclude=\"%s\""
                             acc
                             (string-remove-prefix (concat local-dir "/") elt)))
                   ""
                   (-pinfo-ignore-dirs pinfo))))
    (format "rsync -Pazv %s %s/ %s:%s"
            ignores
            local-dir
            server
            remote-dir)))

(defun -current-buffer-in-project()
  (in-any-project-p buffer-file-name))

(defun devremote-try-transfer-current ()
  (when (and devremote-mode (-current-buffer-in-project))
    (-copy-to-remote buffer-file-name)
    ))

(defun devremote-transfer-current ()
  (interactive)
  (if (-current-buffer-in-project)
      (devremote-try-transfer-current)
    (error-not-in-project buffer-file-name)))


(defun devremote-toggle-auto-transfer ()
  (interactive)
  (let* ((fn 'devremote-transfer-current)
         (enabled (member fn after-save-hook)))

    (if enabled
        (remove-hook 'after-save-hook fn t)
      (add-hook 'after-save-hook fn nil t))

    (if enabled
        (kill-local-variable 'devremote-mode-lighter)
      (setq devremote-mode-lighter
            (format "%s(A)"
                    (symbol-value
                     (make-local-variable 'devremote-mode-lighter)))))))

(defun --devremote-update-lighter()
  (if (-current-buffer-in-project)
      (setq-local devremote-mode-lighter
		  (let ((server (-pinfo-server (devremote-query-pinfo (buffer-file-name)))))
		    (format " DR(%s)" (string-remove-prefix "/ssh:" server)))
		  )))

(defun --devremote-find-file-hook-function()
  (when (-current-buffer-in-project)
    (devremote-mode)
    ))

(add-hook 'find-file-hook '--devremote-find-file-hook-function)

(define-minor-mode devremote-mode
  "Write code on home and test them on server by ssh"
  :lighter devremote-mode-lighter
  :keymap (make-sparse-keymap "devremote")
  :after-hook
  (--devremote-update-lighter)
  )

(defun devremote-create-project(local-root ignores server remote-root cmd)
  (interactive (list
                (read-directory-name "Local Root Directory :"
                                     (projectile-project-root) nil t)
                (split-string
                 (read-string "Local ignore directories (split by space) :" ".git .cache")
                 " " t)
		(--devremote-select-ssh-server)
                (read-directory-name "Remote Root Directory name :" (projectile-project-root))
                (read-shell-command "Remote build command line :" "make")
                ))
  (let ((pinfo (make--pinfo
                :server server
                :local-root-dir local-root
                :ignore-dirs ignores
                :remote-root-dir remote-root
                :build-cmd cmd)))
    (add-to-list 'devremote-project-infos pinfo)
    (devremote-save-known-projects)
    (--devremote-find-file-hook-function)))

(easy-menu-define devremote-menu devremote-mode-map
  "Menus for devremote-mode."
  '("DevRemote"
    ["Create New Project" devremote-create-project]
    ["Compile on remote" devremote-compilation-project]
    ["Transfer Current" devremote-transfer-current]
    ["Automatically Transfer" devremote-toggle-auto-transfer]
    ["Transfer Project" devremote-transfer-project]))

(devremote-load-known-projects)

(provide 'devremote)

;;; devremote.el ends here
