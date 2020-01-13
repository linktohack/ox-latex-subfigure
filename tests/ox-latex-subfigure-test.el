;;; ox-latex-subfigure-test.el --- Test definitions for ox-latex-subfigure  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; URL: https://github.com/linktohack/ox-latex-subfigure

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Test definitions for `ox-latex-subfigure'.


;;; Code:

(require 'buttercup)
(require 'ox-latex-subfigure)

(defconst ox-latex-subfigure-test-dir (file-name-directory
                                       (cond
                                        (load-in-progress load-file-name)
                                        ((and (boundp 'byte-compile-current-file)
                                              byte-compile-current-file)
                                         byte-compile-current-file)
                                        (:else (buffer-file-name)))))

(describe "A suite"
  (it "contains a spec with an expectation"
    (expect t :to-be t)))


;; (provide 'ox-latex-subfigure-test)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; ox-latex-subfigure-test.el ends here
