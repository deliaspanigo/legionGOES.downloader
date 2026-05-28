fn_my_folder_testing <- function(){


  the_folder <- fn_my_folder_package()
  the_testing <-   file.path(dirname(the_folder), "folder_testing")
  return(the_testing)
}
