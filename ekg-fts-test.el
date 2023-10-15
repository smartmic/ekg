;;; ekg-fts-test.el --- Tests for ekg-fts -*- lexical-binding: t; -*-

;; ekg, Copyright (c) 2023  Andrew Hyatt <ahyatt@gmail.com>
;; ekg-fts-test.el, Copyright (c) 2023 Martin Michel <dev@famic.de>: Initial implementation

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

;; Tests for full-text search using SQLite3 FTS5 extension.
;;
;; The choice of sentences and words in the test notes have no further
;; meaning but are simply chosen at random from the Leipzig Corpora
;; Collection 2020 (10k English corpus), see
;; <https://wortschatz.uni-leipzig.de/en/download/>

;;; Code:
(require 'ekg)
(require 'ert)
(require 'ert-x)
(require 'ekg-test-utils)
(require 'ekg-fts)

(defun fts-integrity-check (db)
  "Verify if full-text index is consistent with external content, in
ekg-fts case, a view on the triples table where the predicate is
'text/text'"
    (sqlite-execute db "INSERT INTO ekg_fts(ekg_fts, rank) VALUES ('integrity-check', 1);"))

(ekg-deftest ekg-fts-test-query-after-insert ()
  "Test if a simple query after saving a new note matches"
  (let ((note (ekg-note-create :text "They are asking for a 2.5% wage increase, which is in line with other city workers’ wage increases." :mode 'text-mode :tags '("tag1" "tag2"))))
    (ekg-setup-fts)
    (ekg-save-note note)
    (should (= 1 (length (ekg-get-notes-from-fts "city"))))
    (should (= 0 (length (ekg-get-notes-from-fts "saka"))))))


(ekg-deftest ekg-fts-test-query-after-rebuild ()
  "Test if a simple query after rebuilding the FTS index matches"
  (let ((note (ekg-note-create :text "Eventually, Chuck did redeem himself in the final two seasons of the show." :mode 'text-mode :tags '("tag1" "tag2"))))
    (ekg-save-note note)
    (ekg-setup-fts)
    (ekg-rebuild-fts)
    (fts-integrity-check ekg-db)
    (print ekg-db-file)
    (should (= 1 (length (ekg-get-notes-from-fts "redeem"))))
    (should (= 0 (length (ekg-get-notes-from-fts "jersey"))))))

(ekg-deftest ekg-fts-test-search-trashed-note ()
  "Test if trashing of notes works with FTS"
  (let ((note1 (ekg-note-create :text "The mayor proudly shared that grant awards doubled from 2018 to more than $12 million in 2019, and that employment for East Orange residents had increased by nearly 20 percent." :mode 'text-mode :tags '("tag1" "tag2")))
	(note2 (ekg-note-create :text "The editorial further claims the administration imposed “smaller fines” on nursing homes, though total fines increased 21 percent between fiscal years 2016 and 2019." :mode 'text-mode :tags '("tag2"))))
    (ekg-setup-fts)
    (ekg-save-note note1)
    (ekg-save-note note2)
    (should (= 1 (length (ekg-get-notes-from-fts "doubled"))))
    (should (= 2 (length (ekg-get-notes-from-fts "increased"))))
    (ekg-note-trash note1)
    (should (= 0 (length (ekg-get-notes-from-fts "doubled"))))
    (ekg-rebuild-fts)
    (print ekg-db-file)
    (should (= 0 (length (ekg-get-notes-from-fts "doubled"))))
    (should (= 1 (length (ekg-get-notes-from-fts "increased"))))
    (should (= 1 (length (ekg-get-notes-from-fts "fiscal"))))
    (fts-integrity-check ekg-db)))


(ekg-deftest ekg-fts-test-search-deleted-note ()
  "Testing if deletion of notes works with FTS"
  (let ((note1 (ekg-note-create :text "This is not the first time Nolan and WB have negotiated such as deal, but it is still a rarity typically reserved for upper-echelon filmmakers." :mode 'text-mode :tags '("tag1")))
	(note2 (ekg-note-create :text "Net written premium for the second quarter 2020 increased 11.9% compared to the same period in 2019." :mode 'text-mode :tags '("tag2"))))
    (ekg-setup-fts)
    (ekg-save-note note1)
    (ekg-save-note note2)
    (should (= 1 (length (ekg-get-notes-from-fts "filmmakers"))))
    (ekg-note-delete note1)
    (should (= 0 (length (ekg-get-notes-from-fts "filmmakers"))))
    (print ekg-db-file)
    (should (= 1 (length (ekg-get-notes-from-fts "premium"))))
    (fts-integrity-check ekg-db)))


(ekg-deftest ekg-fts-test-triples-rebuild-db ()
  "Test if rebuilding of database triggered by `ekg-connect' does
 not affect the FTS schema for the notes view"
  (let ((note (ekg-note-create :text "Mullen responded by putting on a for his postgame news conference, almost welcoming a villain role amid increased scrutiny." :mode 'text-mode :tags '("tag1"))))
    (ekg-setup-fts)
    (ekg-save-note note)
    (should (= 1 (length (ekg-get-notes-from-fts "news"))))
    (ekg-close)
    (ekg-connect)
    (should (= 1 (length (ekg-get-notes-from-fts "news"))))
    (fts-integrity-check ekg-db)))


(ekg-deftest ekg-fts-test-query-syntax ()
  "Test FTS5 basic query syntax"
  (let ((notes (list (ekg-note-create :text "The Zambezi River Authority has increased water allocation for power generation operations at Kariba by 4 billion Cubic Meters." :mode 'text-mode :tags '("tag1"))
		     (ekg-note-create :text "The population in Berkeley County has increased more than 25 percent since 2010, which has made land use a huge priority for the council and business community." :mode 'text-mode :tags '("tag2"))
		     (ekg-note-create :text "Mastercard’s \"SpendingPulse\" survey is also estimating that holiday sales only increased 2% vs. the industry’s forecast of a 3.6% to 5.2% rise." :mode 'text-mode :tags '("tag3")))))
    (ekg-setup-fts)
    (mapc #'ekg-save-note notes)
    (print ekg-db-file)
    (should (= 2 (length (ekg-get-notes-from-fts "has increased"))))
    (should (= 2 (length (ekg-get-notes-from-fts "increased has"))))
    (should (= 0 (length (ekg-get-notes-from-fts "increased + has"))))
    (should (= 3 (length (ekg-get-notes-from-fts " increased "))))
    (should (= 2 (length (ekg-get-notes-from-fts "\"has increased\""))))
    (should (= 1 (length (ekg-get-notes-from-fts "\"\"SpendingPulse\"\""))))
    (should (= 0 (length (ekg-get-notes-from-fts "2%"))))
    (should (= 1 (length (ekg-get-notes-from-fts "\"2%\""))))
    (should (= 2 (length (ekg-get-notes-from-fts "has incre*"))))
    (should (= 0 (length (ekg-get-notes-from-fts "\"incre*\""))))
    (should (= 2 (length (ekg-get-notes-from-fts "^the"))))
    (should (= 0 (length (ekg-get-notes-from-fts "NEAR(\"Zambezi River\" \"Cubic Meters\")"))))
    (should (= 1 (length (ekg-get-notes-from-fts "NEAR(\"Zambezi River\" \"Cubic Meters\", 15)"))))
    (should (= 1 (length (ekg-get-notes-from-fts "Authority River Zambezi"))))
    (should (= 0 (length (ekg-get-notes-from-fts "Authority + River + Zambezi"))))
    (should (= 0 (length (ekg-get-notes-from-fts "Authority Berkeley survey"))))
    (should (= 3 (length (ekg-get-notes-from-fts "Authority OR Berkeley OR survey"))))
    (should (= 2 (length (ekg-get-notes-from-fts "Authority AND power OR survey"))))))


;;; Manual testing using interactive ekg with one of the test databases:

;; (ekg-close)
;; (setq ekg-db-file "/tmp/ekg-testberlPn")
;; (ekg-connect)
;; (ekg-get-notes-from-fts "on")
;; (ekg-get-notes-with-tag "tag2")
;; (ekg-db-file)
