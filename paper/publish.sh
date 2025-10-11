#!/usr/bin/env bash
set -euo pipefail

# Paths
QMD="paper.qmd"
PDF="paper.pd"
HTML_SRC="paper.html"
HTML_DST="../index.html"
PDF_DST="../paper.pdf"

echo "Rendering HTML from ${QMD}..."
quarto render "${QMD}" --to html

echo "Copying ${HTML_SRC} -> ${HTML_DST}..."
cp "${HTML_SRC}" "${HTML_DST}"
cp "${PDF}" "${PDF_DST}"
echo "Committing and pushing index.html..."
git add "${HTML_DST}"
git add "${PDF_DST}"
git commit -m "Publish HTML as index.html (auto)"
git push

echo "Done."