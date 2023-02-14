;;; predictable.el --- display buffer in a predictable way  -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Quang-Linh LE

;; Author: Quang-Linh LE<linktohack@gmail.com>
;; Keywords: convinience, frames, window, buffer, window management
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A set of `display-buffer' actions to display buffers in predictable
;; way, bo be used with `display-buffer-alist' and some helpers to
;; manage windows & tabs


;;;; Examples

;; (add-to-list 'display-buffer-alist
;;              `(,(rx bot (or "*Help*" "*helpful" "*info*"))
;;                ((lambda (b a) (message "DISPLAY `%s'" (buffer-name b)) nil)
;;                 ,(predictable-display-buffer-reuse-window-with-predicate (buf)
;;                    (->> buf
;;                         (buffer-name)
;;                         (string-match-p (rx bot (or "*Help" "*helpful" "*info*")))))
;;                 display-buffer-pop-up-window
;;                 predictable-display-buffer-use-largest-window)
;;                (inhibit-same-window . t)))

;;; Code:

;;;; display-buffer actions

;;;###autoload
(defmacro predictable-display-buffer-reuse-window-with-predicate (&rest args)
  "Factory to create `display-buffer' action that tries to find a
window with a `predicate' against a list of buffer `buf' in that window.

The list of buffers to check are in this order

1. The buffers currently displayed in the all windows
2. The previous buffers of each window (one by window)
3. The next buffers of each window
4. The before previous buffer each window
5. The next to next buffer each window
..."
  (declare (indent defun))
  (pcase args
    (`((,arg) . ,body)
     `(lambda (buffer alist)
        (let* ((predicate #'(lambda (,arg) ,@body))
               (alist-entry (assq 'reusable-frames alist))
               (frames (cond (alist-entry (cdr alist-entry))
                             ((if (eq pop-up-frames 'graphic-only)
                                  (display-graphic-p)
                                pop-up-frames)
                              0)
                             (display-buffer-reuse-frames 0)
                             (t (last-nonminibuffer-frame))))
               (inhibit-same-window-p (cdr (assq 'inhibit-same-window alist)))
               (curwin (selected-window))
               (curframe (selected-frame))
               (windows1 (seq-filter 'window-live-p
                                     (window-list-1 nil 'nomini frames)))
               (windows (if inhibit-same-window-p
                            (remove curwin windows1)
                          windows1))
               found)
          (setq found (seq-find #'(lambda (win)
                                    (let ((buf (window-buffer win)))
                                      (funcall predicate buf))) windows))
          (unless found
            (let ((merge-lists (lambda (&rest lists)
                                 (let ((copy (mapcar 'copy-sequence lists))
                                       (result))
                                   (while copy
                                     (dolist (lst copy)
                                       (pcase lst ; should be much simpler if pop does change `lst'
                                         (`(,a ,b . ,rest) (progn
                                                             (push a result)
                                                             (setcar lst b)
                                                             (setcdr lst rest)))
                                         (`(,a) (progn
                                                  (push a result)
                                                  (setcar lst nil)
                                                  (setcar lst nil)))))
                                     (setq copy (remove '(nil) (remove nil copy))))
                                   (nreverse result))))
                  next prev buffers)
              (dolist (win windows)
                (setq next (nconc next (mapcar #'(lambda (buf) (list :win win :buf buf))
                                               (window-next-buffers win))))
                (setq prev (nconc prev (mapcar #'(lambda (buf) (list :win win :buf (car buf)))
                                               (window-prev-buffers win)))))
              (let ((pairs (funcall merge-lists prev next)))
                (setq found (seq-find (pcase-lambda (`,(map :win :buf))
                                        (funcall predicate buf))
                                      pairs)))))
          (when (window-live-p found)
            (prog1 (window--display-buffer buffer found 'reuse alist)
              (unless (cdr (assq 'inhibit-switch-frame alist))
                (window--maybe-raise-frame (window-frame found))))))))
    (_ (error "oops"))))

;;;###autoload
(defun predictable-display-buffer-use-largest-window (buffer alist)
  "Display `buffer' on the largest window."
  (let* ((not-this-window (cdr (assq 'inhibit-same-window alist)))
         (window
          (get-largest-window 'visible nil not-this-window)))
    (when (window-live-p window)
      (prog1
          (window--display-buffer buffer window 'reuse alist)
        (unless (cdr (assq 'inhibit-switch-frame alist))
          (window--maybe-raise-frame (window-frame window)))))))

;;;; tab helpers

;;;###autoload
(defun predictable-delete-window-with-last-tab (tab)
  "Delete window when the last tab buried."
  (let* ((buffer (if (bufferp tab) tab (cdr (assq 'buffer tab))))
         (window (get-buffer-window buffer)))
    (with-selected-window (or window (selected-window))
      (let ((last-tab-p (length< (tab-line-tabs-window-buffers) 2)))
        (if (eq buffer (current-buffer))
            (bury-buffer)
          (set-window-prev-buffers nil (assq-delete-all buffer (window-prev-buffers)))
          (set-window-next-buffers nil (delq buffer (window-next-buffers))))
        (when last-tab-p
          (ignore-errors (delete-window window)))))))

;;;###autoload
(defun predictable-close-all-to-the-right ()
  "Close all others tab to the left"
  (interactive)
  (let* ((win (selected-window)))
    (set-window-prev-buffers win nil)))

;;;###autoload
(defun predictable-close-all-to-the-left ()
  "Close all others tab to the left"
  (interactive)
  (let* ((win (selected-window)))
    (set-window-next-buffers win nil)))

;;;###autoload
(defun predictable-close-others()
  "Close all other tabs"
  (interactive)
  (let* ((win (selected-window)))
    (set-window-next-buffers win nil)
    (set-window-prev-buffers win nil)))


;;;; window helpers

;;;###autoload
(defun predictable-toggle-dedicated-window ()
  "Toggles window dedication in the selected window."
  (interactive)
  (let* ((win (selected-window))
         (dedicated-p (not (window-dedicated-p win)))
         (buf (window-buffer)))
    (set-window-dedicated-p win dedicated-p)
    (message "`%s' dedicated: %s" (buffer-name buf) dedicated-p)))

(provide 'predictable)
;;; predictable.el ends here
