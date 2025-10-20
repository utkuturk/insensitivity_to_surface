# =========================
# Quarto Paper Makefile
# =========================
# Usage:
#   make             # build html+pdf
#   make publish     # copy html->../index.html, commit+push pdf+index.html on main
#   make overleaf_branch  # update 'overleaf' branch with tex, figures, and latex files
#   make clean
#   OPEN=true make open

SHELL := /bin/bash

# Directory of this Makefile (trailing slash)
ROOT      := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

# The Quarto project lives under paper/
BASE_DIR  := $(ROOT)paper/

QUARTO    := quarto
CP        := cp -f
RSYNC     := rsync -av --delete

# Core files
QMD       := $(BASE_DIR)paper.qmd
HTML      := $(BASE_DIR)paper.html
PDF       := $(BASE_DIR)paper.pdf
TEX       := $(BASE_DIR)paper.tex   # produced with keep-tex

# Publish destination is the repo root (index.html at top-level)
PUBLISH_DIR := $(ROOT)
HTML_DST    := $(PUBLISH_DIR)/index.html

# Git repo root
REPO_ROOT := $(shell git -C "$(ROOT)" rev-parse --show-toplevel 2>/dev/null || echo "$(ROOT)")

# Overleaf branch/worktree settings
OVERLEAF_BRANCH ?= overleaf
OVERLEAF_REMOTE ?= origin
OVERLEAF_WORKTREE := $(ROOT).overleaf_worktree

.DEFAULT_GOAL := all

# --------- Build targets ---------
all: html pdf

.PHONY: html pdf
html: $(HTML)
pdf:  $(PDF)

$(HTML): $(QMD)
	$(QUARTO) render "$(QMD)" --to html

$(PDF): $(QMD)
	$(QUARTO) render "$(QMD)" --to elsevier-pdf -M keep-tex=true

# --------- Publish to main branch (html->index.html at repo root, pdf stays in paper/) ---------
.PHONY: publish
publish: html pdf
	@echo "Copying $(HTML) -> $(HTML_DST)"
	$(CP) "$(HTML)" "$(HTML_DST)"
	@echo "Committing & pushing changes on main..."
	@if git -C "$(REPO_ROOT)" diff --quiet -- "$(HTML_DST)" "$(PDF)"; then \
	  echo "No changes to commit."; \
	else \
	  git -C "$(REPO_ROOT)" add -- "$(HTML_DST)" "$(PDF)"; \
	  git -C "$(REPO_ROOT)" commit -m "Publish HTML/PDF (auto)"; \
	  git -C "$(REPO_ROOT)" push; \
	fi
	@echo "Publish complete."

# --------- Overleaf branch via worktree (only minimal artifacts) ---------
RSYNC := rsync -av --delete --delete-excluded

OVERLEAF_INCLUDE := \
  --include='paper.tex' \
  --include='paper_files/figure-pdf/***' \
  --include='*.sty' \
  --include='*.cls' \
  --include='*.bst' \
  --include='*.bib' \
  --include='*.tex' \
  --exclude='*'

.PHONY: overleaf_branch
overleaf_branch: pdf
	@echo "Preparing Overleaf branch '$(OVERLEAF_BRANCH)' via worktree..."
	# pre-clean any stale worktree
	@git -C "$(REPO_ROOT)" worktree remove -f "$(OVERLEAF_WORKTREE)" 2>/dev/null || true
	@rm -rf "$(OVERLEAF_WORKTREE)" 2>/dev/null || true
	@git -C "$(REPO_ROOT)" worktree prune 2>/dev/null || true

	# create (or attach) the worktree for the branch
	@if git -C "$(REPO_ROOT)" show-ref --verify --quiet refs/heads/$(OVERLEAF_BRANCH); then \
	  git -C "$(REPO_ROOT)" worktree add -B "$(OVERLEAF_BRANCH)" "$(OVERLEAF_WORKTREE)" "$(OVERLEAF_BRANCH)"; \
	else \
	  git -C "$(REPO_ROOT)" worktree add -b "$(OVERLEAF_BRANCH)" "$(OVERLEAF_WORKTREE)"; \
	fi

	@echo "Cleaning worktree contents (keeping .git)..."
	@cd "$(OVERLEAF_WORKTREE)" && \
	  find . -mindepth 1 -maxdepth 1 ! -name .git -exec rm -rf {} +

	@echo "Syncing Overleaf artifacts into worktree (root)..."
	@mkdir -p "$(OVERLEAF_WORKTREE)/paper_files/figure-pdf"
	@$(RSYNC) $(OVERLEAF_INCLUDE) \
	  "$(BASE_DIR)"/ "$(OVERLEAF_WORKTREE)/"

	@echo "Committing & pushing Overleaf branch..."
	@cd "$(OVERLEAF_WORKTREE)" && \
	  git add -A && \
	  (git commit -m "Update Overleaf artifacts" || true) && \
	  git push -u "$(OVERLEAF_REMOTE)" "$(OVERLEAF_BRANCH)"

	@echo "Cleaning up Overleaf worktree..."
	@git -C "$(REPO_ROOT)" worktree remove -f "$(OVERLEAF_WORKTREE)" 2>/dev/null || true
	@rm -rf "$(OVERLEAF_WORKTREE)" 2>/dev/null || true
	@git -C "$(REPO_ROOT)" worktree prune 2>/dev/null || true
	@echo "Overleaf branch updated."

# --------- Convenience ---------
.PHONY: open
open: pdf
	@if [ "$(OPEN)" = "true" ]; then \
	  echo "Opening $(PDF)"; \
	  open "$(PDF)"; \
	else \
	  echo "Set OPEN=true to auto-open the PDF."; \
	fi

# --------- Clean LaTeX/knitr scratch files ---------
.PHONY: clean
clean:
	@echo "Cleaning aux/latex scratch files..."
	@rm -f $(BASE_DIR)*.{lof,lot,bbl,gz,toc,log,glo,aux,ist} \
	       $(BASE_DIR)*-knitr.Rnw $(BASE_DIR)*-wordcount.tex \
	       $(BASE_DIR).aux.copy $(BASE_DIR).auxlock \
	       $(BASE_DIR).aux.for  $(BASE_DIR).for.tmp $(BASE_DIR).for
	@echo "Clean complete."
