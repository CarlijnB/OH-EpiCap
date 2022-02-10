#function to show content for menuItem when menuSubItems exist (from https://newbedev.com/show-content-for-menuitem-when-menusubitems-exist-in-shiny-dashboard)
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}