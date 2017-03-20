namespace Auth

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
