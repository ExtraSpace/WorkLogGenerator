Rscript -e "bookdown::render_book('.', output_format = 'bookdown::pdf_book')"
Rscript -e "bookdown::render_book('.', output_format = 'bookdown::gitbook')"
cp WorkLog/*.pdf .