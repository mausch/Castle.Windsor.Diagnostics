[<AutoOpen>]
module prelude

open Microsoft.Glee
open Microsoft.Glee.Drawing
open Microsoft.Glee.GraphViewerGdi
open QuickGraph
open QuickGraph.Glee
open System.Drawing
open System.Drawing.Drawing2D
open System.Drawing.Imaging
open System.Drawing.Text

module Array = 
    let cast a = Array.map unbox a

let assertThrows<'e when 'e :> exn> f =
    let action = Gallio.Common.Action f
    MbUnit.Framework.Assert.Throws<'e> action |> ignore

let toGleeGraph (g: #IEdgeListGraph<_,_>) =
    let populator = g.CreateGleePopulator()
    populator.Compute()
    populator.GleeGraph

let toBitmap (g: Graph) = 
    let renderer = GraphRenderer g
    renderer.CalculateLayout()
    let bmp = new Bitmap(int g.Width, int g.Height, PixelFormat.Format32bppArgb)
    use gr = Graphics.FromImage bmp
    gr.CompositingQuality <- CompositingQuality.HighQuality
    gr.InterpolationMode <- InterpolationMode.HighQualityBilinear
    gr.SmoothingMode <- SmoothingMode.AntiAlias
    gr.TextRenderingHint <- TextRenderingHint.ClearTypeGridFit
    renderer.Render(gr, 0, 0, bmp.Width, bmp.Height)
    bmp