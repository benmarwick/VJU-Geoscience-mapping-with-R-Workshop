
# In this script we will visualize several types of geoscience data

# 1. Grain size, LOI and mag sus data
# 2. Pollen data
# 3. Geochemical data


#------------ load packages  -----------------------------------------------

library(tidyverse)
library(tidypaleo)
library(readxl)
library(here)
library(patchwork)
theme_set(theme_paleo(8))


















#------------ read in the data  -----------------------------------------------


excel_file <- here("data/Data availability - PANGEA_version_3.xlsx")
excel_file_sheet_names <- excel_sheets(excel_file)

sed_data_litho_phases <-
  readxl::read_excel(excel_file,
                     sheet = "Lithology")

sed_data_mag_sus <-
  readxl::read_excel(excel_file,
                     sheet = "Magnetic susceptibility")

sed_data_grain_size <-
  readxl::read_excel(excel_file,
                     sheet = "Grain size")

sed_data_loi <-
  readxl::read_excel(excel_file,
                     sheet = "LOI_organic matter (OM)")

sed_data_pollen <-
  readxl::read_excel(excel_file,
                     sheet = "Pollen")

sed_data_xrf <-
  readxl::read_excel(excel_file,
                     sheet = "XRF scanning")





















# prepare the data ------------------------------------------------------

sed_data_litho_phases_tidy <-
  sed_data_litho_phases %>%
  mutate(unit_sub_unit = ifelse(is.na(`Sub-Unit`),
                                `Unit`,
                                `Sub-Unit`)) %>%
  drop_na(unit_sub_unit) %>%
  separate(`Depth (cm)`,
           into = c("upper_depth",
                    "lower_depth")) %>%
  mutate(across(c(upper_depth,
                 lower_depth),
            parse_number)) %>%
  rowwise() %>%
  mutate(mid_depth = mean(c(upper_depth,
                            lower_depth))) %>%
  select(unit_sub_unit,
         mid_depth,
         upper_depth,
         lower_depth)

# prepare a plot for phase labels
litho_phase_labels <-
  ggplot() +
  geom_text(data = sed_data_litho_phases_tidy,
            aes(x = 0,
                y = mid_depth,
                label = unit_sub_unit)) +
  xlim(0, 0) +
  scale_y_reverse() +
  theme_void() +
  NULL





























#------------ explore the grain size data  ----------------

sed_data_long <-
sed_data_grain_size %>%
select(`Reference depth (cm)`,
       `% Sand`,
       `% Silt`,
       `% Clay`) %>%
  pivot_longer(-`Reference depth (cm)`) %>%
  mutate(name = fct_inorder(name))

sed_data_plot <-
ggplot(sed_data_long) +
  aes(x = value,
      y = `Reference depth (cm)`) +
  geom_hline(yintercept = sed_data_litho_phases_tidy$upper_depth,
             colour = "grey80") +
  geom_lineh() +
  geom_point() +
  scale_y_reverse() +
  facet_abundanceh(vars(name)) +
  labs(x = NULL, y = "Depth (cm)")

sed_data_plot +
  litho_phase_labels +
  plot_layout(widths  = c(1, 0.05))

#------------ explore the LOI and mag sus data  ----------------

geo_data <-
  full_join(sed_data_loi,
            sed_data_mag_sus,
            by = "Reference depth (cm)") %>%
  select(`Reference depth (cm)`,
         `LOI 550 (%)` = `LOI\r\n550 (%)`,
         `Magnetic Susceptibility (SI, *e^-5)`) %>%
  pivot_longer(-`Reference depth (cm)`)

geo_data_plot <-
ggplot(geo_data) +
  aes(x = value, y = `Reference depth (cm)`) +
  geom_hline(yintercept = sed_data_litho_phases_tidy$upper_depth,
             colour = "grey80") +
  geom_lineh() +
  geom_point() +
  scale_y_reverse() +
  facet_geochem_gridh(vars(name)) +
  labs(x = NULL, y = "Depth (cm)")

geo_data_plot +
  litho_phase_labels +
  plot_layout(widths  = c(1, 0.05))


#------------ pollen data  -----------------------------------------------

sed_data_pollen_long <-
  sed_data_pollen %>%
  select(`Depth (cm)`,
         `Fern (%)`,
         `Rice (Gramineae) (%)`,
         `Rice (Gramineae) (%)`,
         `Coffea Robusta (%)`,
         `Pinus (%)`,
         `Fagaceae (%)`,
         `Moraceae (%)`) %>%
  pivot_longer(-`Depth (cm)`,
               names_to = "variable",
               values_to = "value")

# compute CONISS
coniss <-
sed_data_pollen_long %>%
  drop_na() %>%
  nested_data(qualifiers = `Depth (cm)`,
              key = variable,
              value = value) %>%
  nested_chclust_coniss()

sed_data_pollen_long_plot <-
  ggplot(sed_data_pollen_long,
         aes(x = value,
             y = `Depth (cm)`)) +
  geom_areah() +
  scale_y_reverse() +
  facet_abundanceh(vars(variable)) +
  labs(x = "Relative abundance (%)", y = "Depth (cm)")

coniss_plot <- ggplot() +
  layer_dendrogram(coniss, aes(y = `Depth (cm)`)) +
  scale_y_reverse() +
  facet_geochem_gridh(vars("CONISS")) +
  labs(x = NULL) +
  theme(axis.text.y.left = element_blank(), axis.ticks.y.left = element_blank()) +
  labs(y = NULL)

# combine plots
sed_data_pollen_long_plot +
  coniss_plot +
  plot_layout(widths  = c(1, 0.25))

#------------ plot geomchem data  -----------------------------------------------

sed_data_xrf_long <-
  sed_data_xrf %>%
  mutate(
    `log (Si/Ti)` = log(Si / Ti),
    `log (Zr/Ti)` = log(Zr / Ti),
    `log (Rb/Zr)` = log(Rb / Zr),
    `log (S)` = log(S),
    `log (Fe)` = log(Fe),
    `Reference depth (cm)` = `Reference\r\ndepth (cm)`
  ) %>%
  select(
    `Reference depth (cm)`,
    `log (Si/Ti)`,
    `log (Zr/Ti)`,
    `log (Rb/Zr)`,
    `log (S)`,
    `log (Fe)`
  ) %>%
  pivot_longer(-`Reference depth (cm)`,
               names_to = "variable",
               values_to = "values")


# basic geochem plot
sed_data_xrf_long_plot <-
  ggplot(sed_data_xrf_long,
         aes(x = values,
             y = `Reference depth (cm)`)) +
  geom_lineh() +
  scale_y_reverse() +
  facet_geochem_gridh(vars(variable)) +
  labs(y = NULL)

# add litho phases on to the plot

sed_data_xrf_long_plot_lines <-
  sed_data_xrf_long_plot +
  geom_hline(yintercept = sed_data_litho_phases_tidy$upper_depth,
             colour = "grey80")

# combine plots
 sed_data_xrf_long_plot_lines +
   litho_phase_labels +
   plot_layout(widths  = c(1, 0.05))

# -- END -----------------------------------------------------------






























 #------------ other stuff  -----------------------------------------------

# Mean grain size

sed_data_grain_size_plot <-
ggplot(sed_data_grain_size,
       aes(`Reference Depth (cm)`,
           `Mean size \r\n(μm)`)) +
  geom_point() +
  geom_line() +
  xlab("Reference Depth (cm)") +
  ylab("Mean size (μm)")

# LOI

sed_data_sed_data_loi_plot <-
ggplot(sed_data_loi,
       aes(`Reference depth (cm)...8`,
           `LOI\r\n550 (%)`)) +
  geom_point() +
  geom_line() +
  xlab("Reference Depth (cm)") +
  ylab("LOI 550 (%)")

sed_data_sed_data_loi_plot +
  coord_flip() +
  scale_x_reverse()

# combine plots that have similar scales

library(patchwork)
sed_data_grain_size_plot / sed_data_sed_data_loi_plot

# Magnetic Susceptibility

sed_data_mag_sus_plot <-
ggplot(sed_data_mag_sus,
       aes(x = `Reference Depth`,
           y = `Magnetic Susceptibility (SI, *e^-5)`)) +
  geom_point()  +
  geom_line()

sed_data_mag_sus_plot_with_phases <-
sed_data_mag_sus_plot +
geom_vline(data = sed_data_litho_phases_tidy,
                       aes(xintercept = upper_depth),
                      colour = "grey80") +
geom_text(data = sed_data_litho_phases_tidy,
                             aes(x = mid_depth,
                                 y = 0.5,
                                 label = unit_sub_unit))


sed_data_mag_sus_plot_with_phases +
  coord_flip() +
  ylab(bquote('Magnetic Susceptibility (SI'~x10^-5*')')) +
  xlab("Reference Depth (cm)")

sed_data_pollen_long_other <-
  sed_data_pollen %>%
  select(`Depth (cm)`,
         `Sum of Counted\r\n pollen grains`,
         `NAP:AP_ratio`,
         `Gramineae/fern ratio`) %>%
  rename(`Counted pollen grains` = `Sum of Counted\r\n pollen grains`,
         `NAP:AP ratio` = `NAP:AP_ratio`) %>%
  pivot_longer(-`Depth (cm)`,
               names_to = "variable",
               values_to = "value")

sed_data_pollen_long_other_plot <-
  ggplot(sed_data_pollen_long_other,
         aes(x = value,
             y = `Depth (cm)`)) +
  geom_lineh() +
  geom_point() +
  scale_y_reverse() +
  facet_geochem_gridh(vars(variable)) +
  labs(x = NULL, y = NULL)




