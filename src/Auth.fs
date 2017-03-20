namespace Auth

open System
open System.Runtime.Serialization

open Tokeniser
open ReturnControl.Main

module Auth = 
    open System
    open System.Collections.Generic
    open System.IO
    
    type AuthStructure = {
        authToken : Guid
        username : string
        loginTime : System.DateTime
        expiryTime : System.DateTime
    }
    
    type AuthResult =
        | AuthStruct of AuthStructure
        | AuthFailure
    
    type OperationResult =
        | Success
        | AuthExpire
        | AuthError

    type TokenState =
         | InvalidToken
         | ValidToken
         | ExpiredToken
    
    let sessionTime = 100.0 // Session time out after 100 minutes.
    
    let userPassDat = File.ReadAllLines(@"..\..\userpass.dat") |> Array.toList
    
    let splitUserPass (line : string) : (string * string)  =
        let split = line.Split ' '
        let user = split.[0]
        let pass = split.[1]
        (user, pass)
    
    let userMap = List.map splitUserPass userPassDat |> Map.ofList
    
    let authUsers = new Dictionary<Guid,AuthStructure>()

    // Extract token Guid from AuthResult.
    let extractToken( authStruct : AuthResult ) : Guid =
        match authStruct with
              | AuthStruct s -> match authUsers.ContainsKey s.authToken with
                                     | true -> s.authToken
                                     | false -> Guid.Empty
              | _ -> Guid.Empty
    
    // /auth endpoint.
    let authUser (user : string) (pass : string) : AuthResult =
        match Map.tryFind user userMap with
              // Username exists and password is correct
              | Some x when x = pass -> let authRecord = { authToken = Guid.NewGuid(); username = user; loginTime = System.DateTime.UtcNow; expiryTime = System.DateTime.UtcNow.AddMinutes sessionTime }
                                        authUsers.Add(authRecord.authToken,authRecord)
                                        AuthStruct authRecord
              // Username doesn't exist or password is incorrect
              | _ -> AuthFailure

    // /invalidate endpoint.
    let invalidateToken ( token : Guid ) : OperationResult = 
        match token with
              | t when t <> Guid.Empty -> authUsers.Remove t |> ignore
                                          Success
              | _ -> AuthError

    // /check endpoint.
    let checkToken ( token : Guid ) : TokenState =
        match token with
              | t when t <> Guid.Empty -> match authUsers.[t].expiryTime < System.DateTime.UtcNow with
                                                | true -> ExpiredToken
                                                | false -> ValidToken
              | _ -> InvalidToken
        
    // /refresh endpoint.
    let refreshToken ( token : Guid ) : AuthResult = 
         match checkToken token with
               | ValidToken -> let oldRecord = authUsers.[token]
                               authUsers.Remove token |> ignore
                               let newRecord = { authToken = oldRecord.authToken; username = oldRecord.username; loginTime = oldRecord.loginTime; expiryTime = System.DateTime.UtcNow.AddMinutes sessionTime }
                               authUsers.Add(newRecord.authToken,newRecord)
                               AuthStruct oldRecord
               | _ -> AuthFailure

    let runQuery (token : Guid) (query : string) : string =
        "SUCCESSSTRING"

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
        match authUser ap.username ap.password with
              | AuthStruct s -> { username = s.username; authToken = s.authToken; expiry = (s.expiryTime).ToString("dd.MM.yy HH:mm") }
              | AuthFailure -> { username = ""; authToken = Guid.Empty; expiry = (System.DateTime.UtcNow).ToString("dd.MM.yy HH:mm") }

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
        match invalidateToken ip.authToken with
              | Success -> { status = 0 }
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
        match refreshToken rp.authToken with
              | AuthStruct t -> { username = t.username; authToken = t.authToken; expiry = (t.expiryTime).ToString("dd.MM.yy HH:mm") }
              | AuthFailure -> { username = ""; authToken = Guid.Empty; expiry = (System.DateTime.UtcNow).ToString("dd.MM.yy HH:mm") }

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
        match checkToken cp.authToken with
              | ValidToken -> { status = 0 }
              | ExpiredToken ->  { status = 1 }
              | InvalidToken -> { status = 2 }

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
        match checkToken qp.authToken with
              | ValidToken -> { result = runQuery qp.authToken qp.query }
              | _ -> { result = "ERRORSTRING" }