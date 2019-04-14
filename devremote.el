;;; devremote.el --- Write on home and test on remote server in Emacs.
;;; Commentary:
;;    automatically transfer to remote server.
;;

(require 'subr-x)
(require 'dash)

(defun error-not-in-project (file)
  (user-error "The file %s isn't in any project"
	      file))

(cl-defstruct -pinfo
  server
  local-root-dir
  ignore-dirs
  remote-root-dir)

(setq devremote-project-infos
      (list (make--pinfo
             :server "/ssh:sw-sh"
             :local-root-dir "~/codes/node"
             :ignore-dirs '(".git" "out")
             :remote-root-dir "~/snyh/node")

            (make--pinfo
             :server "/ssh:sw-sh"
             :local-root-dir "~/codes/go-sw64"
             :ignore-dirs '(".git" "pkg")
             :remote-root-dir "~/snyh/go-sw64")
            ))

(defun -in-dir(f dir) (string-prefix-p dir f))
(defun -not-in-dir (f dir) (not (-in-dir f dir)))

(defun --expand-ignore-dirs (root dirs)
  (mapcar (lambda (elt) (expand-file-name (concat root "/" elt)))
          dirs))

(defun --pinfo-contain-file (pinfo file)
  "Check whether the PINFO contain FILE.

FILE must in local root directory and must not in any of ignore directories."
  (let ((_local-root (expand-file-name (-pinfo-local-root-dir pinfo)))
        (_ignore-dirs (-pinfo-ignore-dirs pinfo))
        (_file (expand-file-name file)))
    (seq-reduce (lambda (acc elt) (and acc (-not-in-dir _file elt)))
                (--expand-ignore-dirs _local-root _ignore-dirs)
                (-in-dir _file _local-root))))

(defun devremote-query-pinfo (file)
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

(defun --before-execute-cmd (cmd)
  (progn
    (erase-buffer)
    (insert (format "BEGIN %s at\n %s\n"
                    cmd
                    (shell-command-to-string "echo -n $(date)"))))
  )

(defun devremote-transfer-project ()
  (interactive)
  (let* (
         (buffer-name "*DEVREMOTE-SYNC*")
         (long-name buffer-file-name)
         (pinfo (devremote-query-pinfo long-name))
         )
    (unless pinfo (error-not-in-project long-name))
    (let ((cmd (-build-rsync-cmd pinfo)))
      (switch-to-buffer-other-window buffer-name)
      (--before-execute-cmd cmd)
      (start-process-shell-command "DEVREMOTE-SYNC"
                                   buffer-name
                                   cmd))))

(defun devremote-compilation-project ()
  (interactive)
  (let* ((buffer-name "*DEVREMOTE-COMPILATION*")
         (long-name buffer-file-name)
         (pinfo (devremote-query-pinfo long-name)))
    (unless pinfo (error-not-in-project long-name))
    (let* ((remote-root-dir (-pinfo-remote-root-dir pinfo))
           (remote-server (string-remove-prefix "/ssh:" (-pinfo-server pinfo)))
           (remote-cmd "make")
           (cmd (format "ssh %s \"cd %s && %s\""
                        remote-server
                        remote-root-dir
                        remote-cmd
                        )))
      (switch-to-buffer-other-window buffer-name)
      (--before-execute-cmd cmd)
      (start-process-shell-command "DEVREMOTE-SYNC"
                                   buffer-name
                                   cmd))))


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
    (format "echo rsync -Pazv %s %s/ %s:%s"
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

(defun --devremote-find-file-hook-function()
  (when (-current-buffer-in-project)
    (devremote-mode)
    ))

(add-hook 'find-file-hook '--devremote-find-file-hook-function)

(define-minor-mode devremote-mode
  "Write code on home and test them on server by ssh"
  :lighter " DR"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<f12>") 'devremote-transfer-current)
            (define-key map (kbd "<f11>") 'devremote-transfer-project)
            (define-key map (kbd "M-<f12>") 'devremote-compilation-project)
            map)
  )

(provide 'devremote)

;;; devremote.el ends here
