# library(installr)
# 
# copy.packages.between.libraries(from = "D:\\file\\R_library"
#                                 , to = "D:\\file\\R_library_new"
#                                 , ask =T, keep_old = TRUE
#                                 , do_NOT_override_packages_in_new_R = TRUE)
# setwd('d:/')

# old computer
installed.packages()
installed.packages()[,1]
Rpack <- installed.packages()[,1]
save(Rpack, file = 'Rpack.Rdata')

# new computer
data = load('Rpack.Rdata')
Rpack

for (i in Rpack) {
  install.packages(i)
}
