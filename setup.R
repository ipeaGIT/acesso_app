# Function to switch between git remotes in a project

change_remotes <- function(remote, branch = "master") {
  
  command <- sprintf("git branch --set-upstream-to=%s/%s", remote, branch)
  
  shell(command)
  
  
}
