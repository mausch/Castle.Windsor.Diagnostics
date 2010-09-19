module Tests

open MbUnit.Framework
open Castle.Core
open Castle.Core.Configuration
open Castle.Core.Internal
open Castle.MicroKernel
open Castle.MicroKernel.Handlers
open Castle.MicroKernel.Registration
open Castle.Windsor
open QuickGraph
open QuickGraph.Algorithms
open System
open System.Linq

type IComp = interface end
type CompA(z: IComp) = interface IComp
type CompB(z: IComp) = interface IComp
type CompC(z: IComp) = interface IComp
type Comp() = class end
type CompZ() = interface IComp

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

type Node(description: string, node: ComponentModel) = 
    member x.desc = description
    member x.node = node
    interface IEquatable<Node> with
        member x.Equals y = x.desc = y.desc
    interface IComparable<Node> with
        member x.CompareTo y = compare x.desc y.desc
    interface IComparable with
        member x.CompareTo y = compare x.desc (y :?> Node).desc
    override x.Equals y = x.desc = (y :?> Node).desc
    override x.GetHashCode() = x.desc.GetHashCode()

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
    let buildNode (m: ComponentModel) = Node(describe m, m)
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
                                                  |> Seq.map buildNode
                                                  |> Set.ofSeq
                                                  |> Seq.toArray
                                        buildNode h,dep)
                         |> dict
    dependencyDict.ToVertexAndEdgeListGraph(fun kv -> kv.Value |> Seq.map (fun n -> SEquatableEdge(kv.Key,n)))

[<Test>]
let cycleInQuickgraph() =
    use c = new WindsorContainer()
    let reg = [|
                Component.For<IComp>().ImplementedBy<CompA>().Named("a").ServiceOverrides(ServiceOverride.ForKey("z").Eq("b"))
                Component.For<IComp>().ImplementedBy<CompB>().Named("b").ServiceOverrides(ServiceOverride.ForKey("z").Eq("c"))
                Component.For<IComp>().ImplementedBy<CompC>().Named("c").ServiceOverrides(ServiceOverride.ForKey("z").Eq("a"))
              |] |> Array.cast
    c.Register reg |> ignore
    let graph = loadGraphFromKernel c.Kernel
    let dfs = Search.DepthFirstSearchAlgorithm graph
    //dfs.add_TreeEdge (fun n -> printfn "tree edge: %s -> %s" n.Source.desc n.Target.desc)
    dfs.add_BackEdge (fun n -> printfn "Cyclic dependency found: %s -> %s" n.Source.desc n.Target.desc)
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

[<Test>]
let ``find lifestyle issues`` () = 
    use c = new WindsorContainer()
    let reg = [|
                Component.For<IComp>().ImplementedBy<CompZ>().Named("a").LifeStyle.Transient
                Component.For<IComp>().ImplementedBy<CompA>().Named("b").LifeStyle.Singleton.ServiceOverrides(ServiceOverride.ForKey("z").Eq("a"))
              |] |> Array.cast
    c.Register reg |> ignore
    let getLS (n: ComponentModel) = 
        match n.LifestyleType with 
        | LifestyleType.Undefined -> LifestyleType.Singleton 
        | x -> x
    let graph = loadGraphFromKernel c.Kernel
    let dfs = Search.DepthFirstSearchAlgorithm graph
    let visitEdge (n: Node SEquatableEdge) = 
        let l1 = getLS n.Source.node
        let l2 = getLS n.Target.node
        if l1 = LifestyleType.Singleton && l2 = LifestyleType.Transient
            then failwithf "Component '%s' is a singleton depending on component '%s' which is a transient" n.Source.node.Name n.Target.node.Name
    dfs.add_ExamineEdge (EdgeAction(visitEdge))
    dfs.Compute()
    ()