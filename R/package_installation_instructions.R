# How to get the package (caspeR) to work on your computer:

# This is a private package, so you can only access it through following these steps.
# 1. usethis::edit_r_environ()

# 2. Copy and paste only this in the window that pops up, save and close the window
#GITHUB_PAT=04e417a2a53f1e990d655314b479acf544bd855e

# 3. Restart R (you can just hit the Session tab and “Restart R”)

# 4. usethis::github_token() this should show you the token you put in the R environment

# 5. Finally, devtools::install_github(“byuidss/caspeR”) if it asks what packages you want to update, you can just type 3 (“None”) and it will skip this step. It should say “DONE” when it’s done.

# 6. library(caspeR) and it should work!
