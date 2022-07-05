attempts <- read.csv('experiment1AssignmentAttempts.csv')
attempts_clean <- attempts %>% clean_attempts()

events <- read.csv('experiment1Events.csv')
events_clean <- events %>% clean_events()



# output$attempt_table <-
#     DT::renderDT({
#         attempts()
#     })
# 
# output$event_table <-
#     DT::renderDT({
#         events_clean()
#     })
# output$table1 <- DT::renderDT({
#     attempts_clean()
# })
# output$newtable <- DT::renderDT({
#     events_clean()
# })

# tabPanel('Attempts File', DT::DTOutput('attempt_table')),
# tabPanel('Events File', DT::DTOutput('event_table')),
# tabPanel('Summary Table', DT::DTOutput('summary_table')),

# tabPanel('Raw Data', DT::DTOutput('table1')),
# tabPanel('Uploaded Raw', DT::DTOutput('newtable')),
