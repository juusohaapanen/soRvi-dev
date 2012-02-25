
data(MML)

## Original

shp.path <- "/home/jlehtoma/Data/finland/kunnat/kunta1_p_maa_alue.shp"
sp.new <- readShapePoly(shp.path)
sp.new <- PreprocessShapeMML(sp.new)
MML.new <- MML
MML.new[["1_milj_Shape_etrs_shape"]][["kunta1_p"]] <- sp.new
save(MML.new, file="/home/jlehtoma/R/src/soRvi-dev/pkg/data/MMLfull.rda",
     compress="xz")

## 50% simplified

shp.path <- "/home/jlehtoma/Data/finland/kunnat/fi_kunnat.shp"
sp.new <- readShapePoly(shp.path)
sp.new <- PreprocessShapeMML(sp.new)
MML.new <- MML
MML.new[["1_milj_Shape_etrs_shape"]][["kunta1_p"]] <- sp.new
save(MML.new, file="/home/jlehtoma/R/src/soRvi-dev/pkg/data/MMLnew.rda",
     compress="xz")