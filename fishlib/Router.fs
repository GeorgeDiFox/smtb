namespace fishlib

open System.Windows.Forms;

module Router =
    type RouterErrors = 
        | DestinationNotFound
        | ExtraPathNotNeeded
        | RouterNeedsAnExtraPath

    exception DestinationNotFound
    exception ExtraPathNotNeeded
    exception RouterNeedsAnExtraPath
    
    let error_mapper (error: RouterErrors): exn =
        match error with
        | RouterErrors.DestinationNotFound -> DestinationNotFound
        | RouterErrors.ExtraPathNotNeeded -> ExtraPathNotNeeded
        | RouterErrors.RouterNeedsAnExtraPath -> RouterNeedsAnExtraPath

    type Path = 
        | End of string
        | Part of string * Path

    type Destination =
        | Endpoint of View
        | Redirection of Router

    and Router(routes: Map<string, Destination>) =
        let dispatch (path: Path): Result<Destination * Option<Path>, RouterErrors> = 
            let proceed_destination (name: string) (rest: Option<Path>) =
                Map.tryFind name routes 
                 |> Option.map (fun dest -> Ok (dest, rest)) 
                 |> Option.defaultValue (Error RouterErrors.DestinationNotFound)
            match path with
            | End name -> proceed_destination name None
            | Part (name, rest) -> proceed_destination name (Some rest)

        let rec resolve_destination (dest: Destination, rest: Option<Path>): Result<View, RouterErrors> =
            match dest with
            | Endpoint view -> 
                if rest.IsSome then
                    Error RouterErrors.ExtraPathNotNeeded
                else
                    Ok view

            | Redirection router ->
                Option.map Ok rest 
                   |> Option.defaultValue (Error RouterErrors.RouterNeedsAnExtraPath)
                   |> Result.bind route

        and route(path: Path): Result<View, RouterErrors> =
            dispatch path |> Result.bind resolve_destination

        member this.Route(path: Path) : View =
            match route path with
            | Ok view -> view
            | Error err -> raise (error_mapper err)
