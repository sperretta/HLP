namespace Server

open Auth

open Suave
open Suave.Json
open Suave.Filters
open Suave.Operators
open Suave.Successful

module Server =
    let routes =
        choose
              [ POST >=> choose
                 [ path "/auth" >=> ( mapJson ( fun ( ap : Auth.AuthPayload ) -> Auth.AuthProcess ap ) )
                   path "/invalidate" >=> ( mapJson ( fun ( ip : Auth.InvalidatePayload ) -> Auth.InvalidateProcess ip ) )
                   path "/refresh" >=> ( mapJson ( fun ( rp : Auth.RefreshPayload ) -> Auth.RefreshProcess rp ) )
                   path "/query" >=> ( mapJson ( fun ( qp : Auth.QueryPayload ) -> Auth.QueryProcess qp ) )
                   path "/check" >=> ( mapJson ( fun ( cp : Auth.CheckPayload ) -> Auth.CheckProcess cp ) ) ] ]

    let runServer = startWebServer defaultConfig routes
