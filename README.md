# Shiny_Hours_App

This app helps the management team at Greenoug analyze staff hours data. Prior to this, this task was conducted by hand and manually entered into Excel.

It's published publicly because it doesn't do anything until you upload data to it, and the data don't persist (not stored in a database anywhere). All the views and lists are populated automatically from the data the user uploads.

The Overview screen shows alerts by client and staff member (consultant) - people or clients who are on track to use more than their allotted hours for the month are highlighted.

The By Client view shows a bar graph, with the hours used on the client broken down by staff member (x axis) and project (color). Users can select a client to view using the dropdown in the upper left. The By Consultant view does something similar, but with staff member in the dropdown and client on the x axis.

The challenge with this app was linking two different data sets - one from the hours tracking software, and one which is an .xlsx that the managers look at and manually enter their projections into. The data set from the software was very tidy - each row was an observation, each column a categorical (client, staff member) or numerical (time spent) variable.

The .xlsx, since it needed to be human-readable, was very messy. For example, the rows contained both a list of all individual staff members and a list of compound metrics, such as overservice percentage and percent of retainer used. This looks nice for a person, but is difficult to interpret for the software.

The app takes these two inputs, tidies up the projections .xlsx so that the data are easier to work with, performs an analysis (which produces the views), and then reconstructs the .xlsx so that the user can download the actual hours in the same format as the projections (which was a necessary feature, as some managers prefer to view the two large spreadsheets side by side).
