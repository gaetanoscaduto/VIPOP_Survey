require(rio, dplyr)


# dataset_rep = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/VIPOP_SURVEY/dataset_finali_per_analisi/"
# gdrive_code = "G:/.shortcut-targets-by-id/1WduStf1CW98br8clbg8816RTwL8KHvQW/"


data_IT = import(paste0(dataset_rep, "demo_data_", "IT", ".sav"))

data_IT$country = "IT"

data_IT$id__2 = data_IT$id__

data_IT$id__ = paste0("IT_", data_IT$id__2)  


data_FR = import(paste0(dataset_rep, "demo_data_", "FR", ".sav"))

data_FR$country = "FR"

data_FR$id__2 = data_FR$id__

data_FR$id__ = paste0("FR_", data_FR$id__2)  


data_CZ = import(paste0(dataset_rep, "demo_data_", "CZ", ".sav"))

data_CZ$country = "CZ"

data_CZ$id__2 = data_CZ$id__

data_CZ$id__ = paste0("CZ_", data_CZ$id__2)  


data_SW = import(paste0(dataset_rep, "demo_data_", "SW", ".sav"))

data_SW$country = "SW"

data_SW$id__2 = data_SW$id__

data_SW$id__ = paste0("SW_", data_SW$id__2)  


data_POOL = rbind(data_IT, 
                  data_FR,
                  data_CZ,
                  data_SW)

export(data_POOL, paste0(dataset_rep, "demo_data_POOL.sav"))
