// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
(*
open Tokeniser
open ReturnControl.Main
open ExecutionEngine
open Variable
*)

open databaseStructure

open Auth

open Suave
open Suave.Json
open Suave.Filters
open Suave.Operators
open Suave.Successful

open System
open System.Runtime.Serialization

// Auth

[<DataContract>]
type AuthPayload = 
    {
        [<field: DataMember(Name = "username")>]
        username : string;
        [<field: DataMember(Name = "password")>]
        password : string;
    }

[<DataContract>]
type AuthResponse = 
    {
        [<field: DataMember(Name = "username")>]
        username : string;
        [<field: DataMember(Name = "authToken")>]
        authToken : Guid;
        [<field: DataMember(Name = "expiry")>]
        expiry : string;
    }

let AuthProcess ( ap : AuthPayload ) : AuthResponse = 
    match Auth.authUser ap.username ap.password with
          | Auth.AuthStruct s -> { username = s.username; authToken = s.authToken; expiry = (s.expiryTime).ToString("dd.MM.yy HH:mm") }
          | Auth.AuthFailure -> { username = ""; authToken = Guid.Empty; expiry = (System.DateTime.UtcNow).ToString("dd.MM.yy HH:mm") }

// Invalidate

[<DataContract>]
type InvalidatePayload = 
    {
        [<field: DataMember(Name = "authToken")>]
        authToken : Guid;
    }

[<DataContract>]
type InvalidateResponse = 
    {
        [<field: DataMember(Name = "status")>]
        status : int;
    }

let InvalidateProcess ( ip : InvalidatePayload ) : InvalidateResponse = 
    match Auth.invalidateToken ip.authToken with
          | Auth.Success -> { status = 0 }
          | _ -> { status = 1 }

// Refresh

[<DataContract>]
type RefreshPayload = 
    {
        [<field: DataMember(Name = "authToken")>]
        authToken : Guid;
    }

[<DataContract>]
type RefreshResponse = 
    {
        [<field: DataMember(Name = "username")>]
        username : string;
        [<field: DataMember(Name = "authToken")>]
        authToken : Guid;
        [<field: DataMember(Name = "expiry")>]
        expiry : string;
    }

let RefreshProcess ( rp : RefreshPayload ) : RefreshResponse =
    match Auth.refreshToken rp.authToken with
          | Auth.AuthStruct t -> { username = t.username; authToken = t.authToken; expiry = (t.expiryTime).ToString("dd.MM.yy HH:mm") }
          | Auth.AuthFailure -> { username = ""; authToken = Guid.Empty; expiry = (System.DateTime.UtcNow).ToString("dd.MM.yy HH:mm") }

// Check

[<DataContract>]
type CheckPayload = 
    {
        [<field: DataMember(Name = "authToken")>]
        authToken : Guid;
    }

[<DataContract>]
type CheckResult = 
    {
        [<field: DataMember(Name = "status")>]
        status : int;
    }

let CheckProcess ( cp : CheckPayload ) : CheckResult = 
    match Auth.checkToken cp.authToken with
          | Auth.ValidToken -> { status = 0 }
          | Auth.ExpiredToken ->  { status = 1 }
          | Auth.InvalidToken -> { status = 2 }

// Query

[<DataContract>]
type QueryPayload = 
    {
        [<field: DataMember(Name = "authToken")>]
        authToken : Guid;
        [<field: DataMember(Name = "query")>]
        query : string;
    }

[<DataContract>]
type QueryResponse = 
    {
        [<field: DataMember(Name = "result")>]
        result : string;
    }

let QueryProcess ( qp : QueryPayload ) : QueryResponse =
    match Auth.checkToken qp.authToken with
          | Auth.ValidToken -> { result = Auth.runQuery qp.authToken qp.query }
          | _ -> { result = "ERRORSTRING" }

[<EntryPoint>]
let main argv = 
    let database : databaseStructure.database = ref databaseStructure.INilTable
    let routes =
        choose
              [ POST >=> choose
                 [ path "/auth" >=> ( mapJson ( fun ( ap : AuthPayload ) -> AuthProcess ap ) )
                   path "/invalidate" >=> ( mapJson ( fun ( ip : InvalidatePayload ) -> InvalidateProcess ip ) )
                   path "/refresh" >=> ( mapJson ( fun ( rp : RefreshPayload ) -> RefreshProcess rp ) )
                   path "/query" >=> ( mapJson ( fun ( qp : QueryPayload ) -> QueryProcess qp ) )
                   path "/check" >=> ( mapJson ( fun ( cp : CheckPayload ) -> CheckProcess cp ) ) ] ]

    //let result = "CREATE TABLE CHICKEN (column string) ;" |> Tokeniser.getTokens |> UnwrapResultInto Parser.AST.getTree 
    //printfn "%A" result
    //let myDatabase : databaseStructure.database = ref databaseStructure.INilTable
    //printfn "%A" (ExecutionEngine.Create.create "User Table" [("User Name", databaseStructure.boxData.String None);("User ID", databaseStructure.boxData.Int None);("High Score", databaseStructure.boxData.Int None)] myDatabase)
    //printfn "%A" myDatabase

    startWebServer defaultConfig routes
    0 // return an integer exit code
