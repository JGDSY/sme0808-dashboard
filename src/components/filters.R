library(shiny)
library(shinyWidgets)


analysis_type_filter <- div(
    class="filter_set",
    div(
        class="filter_text",
        "Tipo da Analise"
    ),
    div(
        class="filter_selection",
        pickerInput(
            inputId="analysis_type_input",
            label="",
            choices=c("Notificações de SRAG"),
            multiple=TRUE,
            selected=c("Notificações de SRAG"),
            options = pickerOptions(
                actionsBox = TRUE,
                size = 10,
                selectedTextFormat = "count > 3",
                showTick=TRUE,
            ),
        )
    )
)


age_filter <- div(
    class="filter_set",
    div(
        class="filter_text",
        "Idade do Paciente"
    ),
    div(
        class="filter_selection",
        sliderTextInput(
            inputId = "age_window_filter_input",
            label = "",
            choices = c(0:120),
            selected = c(0, 120)
        )
    )
)


ethnicity_filter <- div(
    class="filter_set",
    div(
        class="filter_text",
        "Cor/Raça/Etnia"
    ),
    div(
        class="filter_selection",
        pickerInput(
            inputId="ethnicity_type_input",
            label="",
            choices=c("Branca", "Preta", "Amarela", "Parda", "Indigena", "Não Especificado"),
            multiple=TRUE,
            selected=c("Branca", "Preta", "Amarela", "Parda", "Indigena", "Não Especificado"),
            options = pickerOptions(
                actionsBox = TRUE,
                size = 10,
                selectedTextFormat = "count > 3",
                showTick=TRUE,
                tickIcon="glyphicon-ok"
            ),
        )
    )
)

ethnicity_type_filter <- div(
    class="filter_set",
    div(
        class="filter_text",
        "Comparações"
    ),
    div(
        class="filter_selection",
        awesomeRadio(
            inputId = "ethnicity_type_filter_input",
            label = "",
                choices = c(
                    "Agrupada",
                    "Individual por Estado",
                    "Individual por Cor/Raça/Etnia"
                ),
            selected = "Agrupada"
        ),
    )
)

# Seasonal filters

seasonal_type_filter <- div(
    class="filter_set",
    div(
        class="filter_text",
        "Aspecto Sazonal Desejado"
    ),
    div(
        class="filter_selection",
        pickerInput(
            inputId="seasonal_type_input",
            label="",
            choices=c("Ano", "Mês", "Semana"),
            multiple=FALSE,
            selected=c("Ano"),
            options = pickerOptions(
                actionsBox = TRUE,
                size = 10,
                selectedTextFormat = "count > 3",
                showTick=TRUE,
            ),
        )
    )
)

seasonal_window_year_filter <- div(
    class="filter_set",
    div(
        class="filter_text",
        "Janela Sazonal (Anos)"
    ),
    div(
        class="filter_selection",
        pickerInput(
            inputId="seasonal_window_year_filter_input",
            label="",
            choices=c(2019, 2020, 2021, 2022, 2023),
            multiple=TRUE,
            selected=c(2019, 2020, 2021, 2022, 2023),
            options = pickerOptions(
                actionsBox = TRUE,
                size = 10,
                selectedTextFormat = "count > 5",
                showTick=TRUE,
            )
        ),
    )
)


seasonal_window_yearweek_filter <- div(
    class="filter_set",
    div(
        class="filter_text",
        "Janela Sazonal (Semanas do Ano)"
    ),
    div(
        class="filter_selection",
        pickerInput(
            inputId="seasonal_window_yearweek_filter_input",
            label="",
            choices=c(1:53),
            multiple=TRUE,
            selected=c(1:53),
            options = pickerOptions(
                actionsBox = TRUE,
                size = 10,
                selectedTextFormat = "count > 7",
                showTick=TRUE,
            )
        ),
    )
)

seasonal_window_weekday_filter <- div(
    class="filter_set",
    div(
        class="filter_text",
        "Janela Sazonal (Dias da Semana)"
    ),
    div(
        class="filter_selection",
        pickerInput(
            inputId="seasonal_window_weekday_filter_input",
            label="",
            choices=c("Seg", "Ter", "Qua", "Qui", "Sex", "Sab", "Dom"),
            multiple=TRUE,
            selected=c("Seg", "Ter", "Qua", "Qui", "Sex", "Sab", "Dom"),
            options = pickerOptions(
                actionsBox = TRUE,
                size = 10,
                selectedTextFormat = "count > 7",
                showTick=TRUE,
            )
        ),
    )
)

seasonal_window_months_filter <- div(
    class="filter_set",
    div(
        class="filter_text",
        "Janela Sazonal (Meses do Ano)"
    ),
    div(
        class="filter_selection",
        pickerInput(
            inputId="seasonal_window_months_filter_input",
            label="",
            choices=c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"),
            multiple=TRUE,
            selected=c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"),
            options = pickerOptions(
                actionsBox = TRUE,
                size = 10,
                selectedTextFormat = "count > 7",
                showTick=TRUE,
            )
        ),
    )
)

frequency_type_filter <- div(
    class="filter_set",
    div(
        class="filter_text",
        "Frequencia Desejado"
    ),
    div(
        class="filter_selection",
        pickerInput(
            inputId="frequency_type_input",
            label="",
            choices=c("Dia", "Semana", "Mes"),
            multiple=FALSE,
            selected=c("Dia"),
            options = pickerOptions(
                actionsBox = TRUE,
                size = 10,
                selectedTextFormat = "count > 3",
                showTick=TRUE,
            ),
        )
    )
)

filters1 <- div(
    class="",
    div(
        class="",
        analysis_type_filter,
        frequency_type_filter
        
    )
)

filters2 <- div(
    class="",
    div(
        class="",
        seasonal_type_filter,
        seasonal_window_year_filter,
        seasonal_window_months_filter,
        seasonal_window_yearweek_filter,
        seasonal_window_weekday_filter
    )
)

filters3 <- div(
    class="",
    div(
        class="",
        ethnicity_type_filter,
        age_filter,
        ethnicity_filter,
        
    )
)

map_filter <- div(
    class="",
    div(
        class="",
        leafletOutput("map")
    )
)


