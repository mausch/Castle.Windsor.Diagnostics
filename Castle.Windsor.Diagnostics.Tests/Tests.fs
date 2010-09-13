module Tests

open MbUnit.Framework
open Castle.Core
open Castle.MicroKernel
open Castle.MicroKernel.Handlers
open Castle.MicroKernel.Registration
open Castle.Windsor
open QuickGraph
open QuickGraph.Algorithms
open System.Linq

type IComp = interface end
type CompA(z: IComp) = interface IComp
type CompB(z: IComp) = interface IComp
type CompC(z: IComp) = interface IComp

[<Test>]
let cycle() =
    use c = new WindsorContainer()
    let reg = [|
                Component.For<IComp>().ImplementedBy<CompA>().Named("a").ServiceOverrides(dict ["z","b"])
                Component.For<IComp>().ImplementedBy<CompB>().Named("b").ServiceOverrides(dict ["z","c"])
                Component.For<IComp>().ImplementedBy<CompC>().Named("c").ServiceOverrides(dict ["z","a"])
              |] |> Array.cast
    c.Register reg |> ignore
    assertThrows<HandlerException>(fun() -> c.Resolve<IComp>("a") |> ignore)

let loadGraphFromKernel (k: IKernel) = 
    let handlers = k.GetAssignableHandlers typeof<obj>
    let descType o = o.GetType().AssemblyQualifiedName
    let descLS (t: LifestyleType) = 
        let t = match t with 
                | LifestyleType.Undefined -> LifestyleType.Singleton 
                | _ -> t
        t.ToString()
    let describe (m: ComponentModel) = sprintf "%s;%s;%s;%s" m.Name (descType m.Service) (descType m.Implementation) (descLS m.LifestyleType)
    let describeHandler (h: IHandler) = describe h.ComponentModel
    let dependencyDict = handlers 
                         |> Seq.distinctBy describeHandler 
                         |> Seq.map (fun h -> h.ComponentModel)
                         |> Seq.map (fun h ->
                                        let dep = h.Dependencies 
                                                  |> Seq.filter (fun m -> m.DependencyType <> DependencyType.Parameter)
                                                  |> Seq.map (fun m -> k.GetHandler(m.DependencyKey).ComponentModel)
                                                  |> Seq.map describe
                                                  |> Seq.toArray
                                        describe h,dep)
                         |> dict
    dependencyDict.ToVertexAndEdgeListGraph(fun kv -> kv.Value |> Seq.map (fun n -> SEquatableEdge(kv.Key,n)))

[<Test>]
let cycleInQuickgraph() =
    use c = new WindsorContainer()
    let reg = [|
                Component.For<IComp>().ImplementedBy<CompA>().Named("a").ServiceOverrides(dict ["z","b"])
                Component.For<IComp>().ImplementedBy<CompB>().Named("b").ServiceOverrides(dict ["z","c"])
                Component.For<IComp>().ImplementedBy<CompC>().Named("c").ServiceOverrides(dict ["z","a"])
              |] |> Array.cast
    c.Register reg |> ignore
    let graph = loadGraphFromKernel c.Kernel
    let dfs = Search.DepthFirstSearchAlgorithm graph
    //dfs.add_DiscoverVertex (fun n -> printfn "%s" n)
    dfs.add_TreeEdge (fun n -> printfn "%s -> %s" n.Source n.Target)
    dfs.add_ForwardOrCrossEdge (fun n -> printfn "%s -> %s" n.Source n.Target)
    dfs.Compute()
    ()    

[<Test>]
let paintGraph() = 
    use c = new WindsorContainer()
    let reg = [|

                Component.For<IComp>().ImplementedBy<CompA>().Named("a").ServiceOverrides(dict ["z","b"])
                Component.For<IComp>().ImplementedBy<CompB>().Named("b").ServiceOverrides(dict ["z","c"])
                Component.For<IComp>().ImplementedBy<CompC>().Named("c").ServiceOverrides(dict ["z","a"])
              |] |> Array.cast
    c.Register reg |> ignore
    let graph = loadGraphFromKernel c.Kernel
    let bmp = graph |> toGleeGraph |> toBitmap
    bmp.Save @"c:\graph.png"
    ()

    