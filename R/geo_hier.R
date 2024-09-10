#' Creating Ghana's Geographic Hierarchy
#'
#' @return Output is a sdc hierarchy of the regions, districts and subdistricts of Ghana.
#' @export
#'
#' @examples
#' geo_hier()
geo_hier <- function(){
  # Create geo_hier
  geo_hier <- districts_order |>
    dplyr::rename(District = subdist_name) |>
    dplyr::left_join(region_district) |>
    dplyr::filter(!is.na(Region)) |>
    dplyr::mutate(Region = factor(Region, levels = c("Western", "Central","Greater Accra" ,
                                              "Volta","Eastern","Ashanti",
                                              "Western North", "Ahafo", "Bono",
                                              "Bono East","Oti",
                                              "Northern","Savannah", "North East",
                                              "Upper East", "Upper West"))) |>
    dplyr::arrange(Region, order) |>
    dplyr::transmute(pathString=paste("Ghana",
                               Region,
                               District,
                               sep = "_")) |>
    # Use this pathString to make a hierachy in form of a tree
    data.tree::FromDataFrameTable(pathDelimiter = "_") |>
    #then we make the tree to a table where we have the name and level
    data.tree::ToDataFrameTree("name","level") |>
    #Then we set level one to have one @, level 2 has @@. the number of @ shows the level number
    dplyr::mutate(level = dplyr::case_when(level ==1 ~ '@',
                             level ==2 ~ '@@',
                             level ==3 ~ '@@@')) |>
    #Selects the level and name
    dplyr::select(level,name) |>
    sdcHierarchies::hier_import(from = "df",
                                keep_order = TRUE)

  # Add subdistricts
  geo_hier <- sdcHierarchies::hier_add(geo_hier,
                       root = "Sekondi Takoradi Metropolitan Area (STMA)",
                       nodes = c("STMA-Takoradi",
                                 "STMA-Sekondi",
                                 "STMA-Essikado-Ketan"))
  geo_hier <- sdcHierarchies::hier_add(geo_hier,
                       root = "Cape Coast Metropolitan Area (CCMA)",
                       nodes = c("CCMA-Cape Coast South",
                                 "CCMA-Cape Coast North"))
  geo_hier <- sdcHierarchies::hier_add(geo_hier,
                       root = "Accra Metropolitan Area (AMA)",
                       nodes = c("AMA-Ablekuma South",
                                 "AMA-Ashiedu Keteke",
                                 "AMA-Okaikoi South"))
  geo_hier <- sdcHierarchies::hier_add(geo_hier,
                       root = "Tema Metropolitan Area (TMA)",
                       nodes = c("TMA-Tema Central",
                                 "TMA-Tema East"))
  geo_hier <- sdcHierarchies::hier_add(geo_hier,
                       root = "Tamale Metropolitan Area (TMA)",
                       nodes = c("TMA-Tamale South",
                                 "TMA-Tamale Central"))
  geo_hier <- sdcHierarchies::hier_add(geo_hier,
                       root = "Kumasi Metropolitan Area (KMA)",
                       nodes = c("KMA-Nhyiaeso",
                                 "KMA-Subin",
                                 "KMA-Manhyia South",
                                 "KMA-Manhyia North",
                                 "KMA-Bantama"))

  return(geo_hier)
}
