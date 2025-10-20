#!/usr/bin/env bash
set -euo pipefail

# Paths
BASE_DIR="/Users/utkuturk/shalllow_processing_paper/paper"
QMD="${BASE_DIR}/paper.qmd"
PDF_SRC="${BASE_DIR}/paper.pdf"
HTML_SRC="${BASE_DIR}/paper.html"
HTML_DST="${BASE_DIR}/../index.html"
PDF_DST="${BASE_DIR}/paper.pdf"

echo "Rendering HTML from ${QMD}..."
quarto render "${QMD}" --to html,elsevier-pdf

# # Add options and make glossaries
# echo "Setting options and making glossaries..."
# Rscript -e "options(tinytex.clean = FALSE)
# makeglossaries("${QMD}")

echo "Copying ${HTML_SRC} -> ${HTML_DST}..."
cp -rf "${HTML_SRC}" "${HTML_DST}"
echo "Copying ${PDF_SRC} -> ${PDF_DST}..."
cp -rf "${PDF_SRC}" "${PDF_DST}"
echo "Committing and pushing index.html..."
git add "${HTML_DST}"
git add "${PDF_DST}"
git commit -m "Publish HTML as index.html (auto)"
git push

echo "Done."

# Clean up temporary files
echo "Cleaning up temporary files..."
rm -f "${BASE_DIR}"/*.lof "${BASE_DIR}"/*.lot "${BASE_DIR}"/*.bbl "${BASE_DIR}"/*.gz "${BASE_DIR}"/*.toc "${BASE_DIR}"/*.log "${BASE_DIR}"/*.glo "${BASE_DIR}"/*.aux "${BASE_DIR}"/*.ist "${BASE_DIR}"/*-knitr.Rnw "${BASE_DIR}"/*-wordcount.tex "${BASE_DIR}"/*.aux.copy "${BASE_DIR}"/*.auxlock "${BASE_DIR}"/*.aux.for "${BASE_DIR}"/*.for.tmp "${BASE_DIR}"/*.for

# Check if Overleaf is set
if [ "${OVERLEAF:-}" = "true" ]; then
    echo "Copying files to Overleaf..."
    cp -R "${l_text}/" "${l_overleaf}"
fi

# Check if open is set
if [ "${OPEN:-}" = "true" ]; then
    echo "Opening main-knitr.pdf..."
    open "paper.pdf"
fi
