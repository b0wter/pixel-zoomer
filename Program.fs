open System
open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open b0wter.FSharp

let scaleParameter = "--scale"
let outputParameter = "--output"

type ParserResult<'a>
    = Success of 'a
    | Failure of string
    | NotSet

type ParserParameter<'a> = {
    Argument: string
    Parse: string -> Result<'a, string>
}

let scaleParserParameter = 
    { Argument = scaleParameter 
      Parse = (fun s -> 
        let success, result = Int32.TryParse(s)
        if success then Ok result else Error "Could not parse the scale parameter as an integer."
    )}

let outputParserParameter =
    { Argument = outputParameter
      Parse = Ok }

type Parameter = {
    Scale: int
    Output: string
    Input: string
}

let loadImage (filename: string) =
    try
        Ok (Image.Load filename)
    with
    | ex -> Error ex.Message

let checkFileExistence f : Result<unit, string> =
    if File.Exists(f) then Ok ()
    else Error (sprintf "The file '%s' does not exist." f)

let resize (scale: int) (image: Image) =
    if scale >= 1 then
        do image.Mutate (fun i -> i.Resize(image.Width * scale, image.Height * scale, KnownResamplers.NearestNeighbor) |> ignore)
        Ok image
    else Error "You cannot use a scaling factor smaller than 1."

let saveImage filename (image: Image) =
    image.Save filename

let findParameter<'a> (args: string array) (parameter: ParserParameter<'a>) : ParserResult<'a> =
    let index = Array.IndexOf(args, parameter.Argument)
    if index = -1 then ParserResult.NotSet
    else match (args.[index + 1] |> parameter.Parse) with
         | Ok o -> ParserResult.Success o
         | Error e -> ParserResult.Failure e
    
let findFallbackFilename (f: string) =
    let rec step i name extension =
        let combined = sprintf "%s_%i%s" name i extension
        if File.Exists(combined) then step (i + 1) name extension
        else combined
    step 1 (Path.GetFileNameWithoutExtension(f)) (Path.GetExtension(f))

let addOutputExtensionIfRequired (p: Parameter) : Parameter =
    let inputExtension = Path.GetExtension(p.Input)
    let outputExtension = Path.GetExtension(p.Output)

    if outputExtension |> String.isNullOrWhiteSpace then 
        let extension = if inputExtension |> String.isNullOrWhiteSpace then ".png" else inputExtension
        do printfn "The output filename does not have an extension. This causes problems with the image library. The extension '%s' has been added." extension
        let filename = (p.Output + extension)
        { p with Output = filename}
    else
        p

let parametersFromArgs (args: string array) : Result<Parameter, string> =
    if args.Length = 1 || args.Length = 3 || args.Length = 5 then
        let input = checkFileExistence args.[0]
        let scale = match (findParameter args scaleParserParameter) with Success s -> Ok s | Failure e -> Error e | NotSet -> Ok 2
        let output = match (findParameter args outputParserParameter) with Success s -> Ok s | Failure e -> Error e | NotSet -> Ok (findFallbackFilename args.[0])
        match input, scale, output with
        | Ok _, Ok s, Ok o -> Ok { Input = args.[0]; Output = o; Scale = s }
        | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e
    else
        Error "You need to run the program with the following parameters: $input_filename [--scale $integer = 2] [--output $output_filename]"


let printParameters (p: Parameter) : Parameter =
    printfn "input: %s" p.Input
    printfn "output: %s" p.Output
    printfn "scale: %i" p.Scale
    p

[<EntryPoint>]
let main argv =
    let pipeline = fun (p: Parameter) -> (loadImage p.Input) |> Result.bind (resize p.Scale) |> Result.map (saveImage p.Output)
    let result = match (parametersFromArgs argv) |> Result.map addOutputExtensionIfRequired |> Result.map printParameters |> (Result.bind pipeline) with
                 | Ok _ -> ""
                 | Error e -> e
    do Console.WriteLine(result)
    0