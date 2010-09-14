module Tests

open MbUnit.Framework
open Castle.Core
open Castle.Core.Configuration
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
                Component.For<IComp>().ImplementedBy<CompA>().Named("a").ServiceOverrides(ServiceOverride.ForKey("z").Eq("b"))
                Component.For<IComp>().ImplementedBy<CompB>().Named("b").ServiceOverrides(ServiceOverride.ForKey("z").Eq("c"))
                Component.For<IComp>().ImplementedBy<CompC>().Named("c").ServiceOverrides(ServiceOverride.ForKey("z").Eq("a"))
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
    let describe (m: ComponentModel) = sprintf "%s\n%s\n%s\n%s" m.Name (descType m.Service) (descType m.Implementation) (descLS m.LifestyleType)
    let describeHandler (h: IHandler) = describe h.ComponentModel
    let getServiceOverrides (m: ComponentModel) =
        m.Configuration.Children
        |> Seq.cast<IConfiguration>
        |> Seq.filter (fun c -> c.Name = "parameters")
        |> Seq.collect (fun c -> c.Children)
        |> Seq.cast<IConfiguration>
        |> Seq.filter (fun c -> c.Value.StartsWith("${"))
        |> Seq.map (fun c -> c.Value.[2 .. c.Value.Length-2])

    let getOverridesComponents (k: IKernel) (m: ComponentModel) = 
        getServiceOverrides m
        |> Seq.map k.GetHandler
        |> Seq.filter ((<>) null)
        |> Seq.map (fun h -> h.ComponentModel)

    let dependencyDict = handlers 
                         |> Seq.distinctBy describeHandler 
                         |> Seq.map (fun h -> h.ComponentModel)
                         |> Seq.map (fun h ->
                                        let dep = h.Dependencies 
                                                  |> Seq.filter (fun m -> m.DependencyType <> DependencyType.Parameter)
                                                  |> Seq.map (fun m -> k.GetHandler(m.DependencyKey).ComponentModel)
                                                  |> Seq.append (getOverridesComponents k h)
                                                  |> Seq.map describe
                                                  |> Set.ofSeq
                                                  |> Seq.toArray
                                        describe h,dep)
                         |> dict
    dependencyDict.ToVertexAndEdgeListGraph(fun kv -> kv.Value |> Seq.map (fun n -> SEquatableEdge(kv.Key,n)))

[<Test>]
let cycleInQuickgraph() =
    use c = new WindsorContainer()
    let reg = [|
                Component.For<IComp>().ImplementedBy<CompA>().Named("a").ServiceOverrides(ServiceOverride.ForKey("z").Eq("b"))
                Component.For<IComp>().ImplementedBy<CompB>().Named("b").ServiceOverrides(ServiceOverride.ForKey("z").Eq("c"))
                Component.For<IComp>().ImplementedBy<CompC>().Named("c")
                Component.For<IComp>().ImplementedBy<CompC>().Named("d").ServiceOverrides(ServiceOverride.ForKey("z").Eq("b"))
              |] |> Array.cast
    c.Register reg |> ignore
    let graph = loadGraphFromKernel c.Kernel
    let dfs = Search.DepthFirstSearchAlgorithm graph
    dfs.add_TreeEdge (fun n -> printfn "tree edge: %s -> %s" n.Source n.Target)
    dfs.add_BackEdge (fun n -> printfn "back edge Cyclic dependency found: %s -> %s" n.Source n.Target)
    dfs.Compute()
    ()    

[<Test>]
let paintGraph() = 
    use c = new WindsorContainer()
    let reg = [|

                Component.For<IComp>().ImplementedBy<CompA>().Named("a").ServiceOverrides(ServiceOverride.ForKey("z").Eq("b"))
                Component.For<IComp>().ImplementedBy<CompB>().Named("b").ServiceOverrides(ServiceOverride.ForKey("z").Eq("c"))
                Component.For<IComp>().ImplementedBy<CompC>().Named("c").ServiceOverrides(ServiceOverride.ForKey("z").Eq("a"))
              |] |> Array.cast
    c.Register reg |> ignore
    let graph = loadGraphFromKernel c.Kernel
    let bmp = graph |> toGleeGraph |> toBitmap
    bmp.Save @"c:\graph.png"
    ()

    