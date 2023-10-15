;;; ekg-fts.el --- Using full text search within ekg -*- lexical-binding: t -*-

;; ekg, Copyright (c) 2023  Andrew Hyatt <ahyatt@gmail.com>
;; ekg-fts.el, Copyright (c) 2023 Martin Michel <dev@famic.de>: Initial implementation

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ekg-fts provides full-text search capabilities for ekg notes . This
;; works by leveraging the native SQLite3 FTS5 functionality and
;; requires creation of index tables, views and triggers in the
;; `ekg-db-file' database. By using external content, only indices are
;; created and no copy of the text bodies from notes is stored in the
;; database.

(require 'ekg)

(defun ekg-setup-fts ()
  "Setup full text search for EKG.

This command adds objects to the database schema which enable
full-text search using SQLite3 native FTS5 extension."
  (let ((db (sqlite-open ekg-db-file)))
    (sqlite-execute db "CREATE VIEW ekg_note_view AS WITH trash AS (SELECT DISTINCT subject FROM triples WHERE predicate = 'tagged/tag' AND substr(object,2,6) = 'trash/') SELECT rowid, subject, object FROM triples WHERE predicate = 'text/text' AND subject NOT IN (SELECT subject FROM trash);")
    (sqlite-execute db "CREATE VIRTUAL TABLE IF NOT EXISTS ekg_fts USING FTS5 (subject, object, content = 'ekg_note_view', content_rowid = rowid);")
    (sqlite-execute db "CREATE TRIGGER IF NOT EXISTS ekg_fts_ai AFTER INSERT ON triples WHEN new.predicate = 'text/text' BEGIN
  INSERT INTO ekg_fts(rowid, subject, object) VALUES (new.rowid, new.subject, new.object);
END;")
    (sqlite-execute db "CREATE TRIGGER IF NOT EXISTS ekg_fts_au AFTER UPDATE ON triples WHEN old.predicate = 'tagged/tag' AND substr(new.object,2,6) = 'trash/' BEGIN
  INSERT INTO ekg_fts(ekg_fts, rowid, subject, object) SELECT 'delete', rowid, subject, object FROM triples WHERE subject = old.subject AND predicate = 'text/text';
END;")
    (sqlite-execute db "CREATE TRIGGER IF NOT EXISTS ekg_fts_ad AFTER DELETE ON triples WHEN old.predicate = 'text/text' BEGIN
  INSERT INTO ekg_fts(ekg_fts, rowid, subject, object) VALUES('delete', old.rowid, old.subject, old.object);
END;")))

(defun ekg-rebuild-fts ()
  "First delete the entire full-text index, then rebuild it based on
the external content view.

This command comes in handy if the FTS index and its external content
 (a view of triples table where predictate is `text/text') have
become inconsistent. It can also be used to initially populate
the FTS index."
  (let ((db (sqlite-open ekg-db-file)))
    (sqlite-execute db "INSERT INTO ekg_fts(ekg_fts) VALUES('rebuild');")))

(defun ekg-get-notes-from-fts (query)
  "Get all notes containing QUERY in its body, returning a list
 of `ekg-notes' structs."
  (ekg-connect)
  (let ((ids-from-fts
	 (mapcar (lambda (result)
		   (if (stringp (car result))
		       (funcall #'string-trim (car result) "\"" "\"")
		     (car result)))
		 (sqlite-select (sqlite-open ekg-db-file) "SELECT subject FROM ekg_fts WHERE ekg_fts MATCH ? ORDER BY rank" (list (format "%s" query))))))
    (mapcar #'ekg-get-note-with-id ids-from-fts)))

(defun ekg-show-notes-from-fts (query)
  "Show notes matching a full text search query in an `ekg-notes' buffer."
  (interactive "sQuery: ")
  (ekg-setup-notes-buffer
   (format "full text search: %s" query)
   (lambda () (sort (ekg-get-notes-from-fts query) #'ekg-sort-by-creation-time))
   nil))

(provide 'ekg-fts)
