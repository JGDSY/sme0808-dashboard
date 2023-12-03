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
            choices=c("Notificações de SRAG", "Obitos", "Internação", "Internação em UTI"),
            multiple=FALSE,
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


year_filter <- div(
    class="filter_set",
    div(
        class="filter_text",
        "Intervalo de Anos"
    ),
    div(
        class="filter_selection",
        sliderTextInput(
            inputId = "year_filter_input",
            label = "",
            choices = c(2012:2023),
            selected = c(2012, 2023)
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
                    "Individual por Cor/Raça/Etnia",
                    "Individual por Tipo de SRAG"
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
            choices=c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023),
            multiple=TRUE,
            selected=c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023),
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

sickness_filter <- div(
    class="filter_set",
    div(
        class="filter_text",
        "Sintomas Reportados"
    ),
    div(
        class="filter_selection",
        pickerInput(
            inputId="sickness_filter_input",
            label="",
            choices=c("Febre", "Tosse", "Dor de Garganta", "Dispneia", "Desconforto Respiratorio", "Saturação Baixa", "Outros"),
            multiple=TRUE,
            selected=c(),
            options = pickerOptions(
                actionsBox = TRUE,
                size = 10,
                selectedTextFormat = "count > 3",
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


state_filter <- div(
    class="filter_set",
    div(
        class="filter_text",
        "Estados"
    ),
    div(
        class="filter_selection",
        pickerInput(
            inputId="state_filter_input",
            label="",
            choices=c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO"),
            multiple=TRUE,
            selected=c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO"),
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
        "Frequencia Desejada"
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


frequency_average_type <- div(
    class="filter_set",
    div(
        class="filter_text",
        "Filtros de Tendencia"
    ),
    div(
        class="filter_selection",
        pickerInput(
            inputId="frequency_average_type_input",
            label="",
            choices=c("Nenhum", "Média Movel", "Diferenciação"),
            multiple=FALSE,
            selected=c(),
            options = pickerOptions(
                actionsBox = TRUE,
                size = 10,
                selectedTextFormat = "count > 3",
                showTick=TRUE,
            )
        ),
    )
)

frequency_moving_average_window <- div(
    class="filter_set",
    div(
        class="filter_text",
        "Janela (Dias):"
    ),
    div(
        class="filter_selection",
        numericInput("frequency_moving_average_window_input", "", 10, min = 1, max = 1000),

    )
)

frequency_differentiation_order <- div(
    class="filter_set",
    div(
        class="filter_text",
        "Ordem de Diferenciação:"
    ),
    div(
        class="filter_selection",
        numericInput("frequency_differentiation_order_input", "", 1, min = 1, max = 10),

    )
)

lag_type_filter <- div(
    class="filter_set",
    div(
        class="filter_text",
        "Frequencia da Defasagem:"
    ),
    div(
        class="filter_selection",
        pickerInput(
            inputId="lag_input",
            label="",
            choices=c("Diario", "Mensal"),
            multiple=FALSE,
            selected=c("Diario"),
            options = pickerOptions(
                actionsBox = TRUE,
                size = 10,
                selectedTextFormat = "count > 3",
                showTick=TRUE,
            )
        ),

    )
)



filters1 <- div(
    class="",
    div(
        class="",
        analysis_type_filter,
        year_filter,
        
    )
)

filters2 <- div(
    class="",
    div(
        class="",
        state_filter,
        sickness_filter
        # seasonal_type_filter,
        # seasonal_window_year_filter,
        # seasonal_window_months_filter,
        # seasonal_window_yearweek_filter,
        # seasonal_window_weekday_filter
    )
)

filters3 <- div(
    class="",
    div(
        class="",
        
        age_filter,
        ethnicity_filter,
        
    )
)

map_filter <- div(
    class="",
    div(
        class="",
        # leafletOutput("map")
        ethnicity_type_filter,
    )
)
