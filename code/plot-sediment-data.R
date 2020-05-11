
# First we must install the main pkg that will help us
# If you have to answer questions: 1 (All) and then 'no'

# install.packages("remotes")
# remotes::install_github("paleolimbot/tidypaleo")

#------------ load packages  -----------------------------------------------


library(tidyverse)
library(tidypaleo)
library(readxl)
theme_set(theme_paleo(8))

#------------ read in the data  -----------------------------------------------


excel_file <- "data/Data availability - PANGEA_version_3.xlsx"
excel_file_sheet_names <- excel_sheets(excel_file)

sed_data_litho_phases <-
  readxl::read_excel("data/Data availability - PANGEA_version_3.xlsx",
                     sheet = "Lithology")

sed_data_mag_sus <-
  readxl::read_excel("data/Data availability - PANGEA_version_3.xlsx",
                     sheet = "Magnetic susceptibility")

sed_data_grain_size <-
  readxl::read_excel("data/Data availability - PANGEA_version_3.xlsx",
                     sheet = "Grain size")

sed_data_loi <-
  readxl::read_excel("data/Data availability - PANGEA_version_3.xlsx",
                     sheet = "LOI_organic matter (OM)")

sed_data_pollen <-
  readxl::read_excel("data/Data availability - PANGEA_version_3.xlsx",
                     sheet = "Pollen")

sed_data_xrf <-
  readxl::read_excel("data/Data availability - PANGEA_version_3.xlsx",
                     sheet = "XRF scanning")


#------------ explore the data  -----------------------------------------------

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

sed_data_pollen_long_plot <-
ggplot(sed_data_pollen_long,
       aes(x = value,
           y = `Depth (cm)`)) +
  geom_areah() +
  scale_y_reverse() +
  facet_abundanceh(vars(variable)) +
  labs(x = "Relative abundance (%)", y = "Depth (cm)")

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


# combine plots
library(patchwork)
sed_data_pollen_long_plot + sed_data_pollen_long_other_plot


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
       aes(y = values,
           x = `Reference depth (cm)`)) +
  geom_line() +
  scale_y_reverse() +
  facet_grid(vars(variable),
             scales = "free_y") +
  labs(y = NULL)

# add litho phases on to the plot

sed_data_litho_phases_tidy <-
  sed_data_litho_phases %>%
  mutate(unit_sub_unit = ifelse(is.na(`Sub-Unit`),
                                `Unit`,
                                `Sub-Unit`)) %>%
  drop_na(unit_sub_unit) %>%
  separate(`Depth (cm)`,
           into = c("upper_depth",
                    "lower_depth")) %>%
  mutate_at(vars(upper_depth,
                 lower_depth),
            parse_number) %>%
  rowwise() %>%
  mutate(mid_depth = mean(c(upper_depth,
                            lower_depth))) %>%
  select(unit_sub_unit,
         mid_depth,
         upper_depth,
         lower_depth)

sed_data_xrf_long_plot_lines <-
sed_data_xrf_long_plot +
  geom_vline(xintercept = sed_data_litho_phases_tidy$upper_depth,
             colour = "grey80") +
  xlim(0, 180)

litho_phase_labels <-
ggplot() +
  geom_text(data = sed_data_litho_phases_tidy,
            aes(x = mid_depth,
                y = 0,
                label = unit_sub_unit)) +
  xlim(0, 180) +
 theme_void() +
  NULL

library(patchwork)
litho_phase_labels / sed_data_xrf_long_plot_lines + plot_layout(heights = c(0.05, 1))




