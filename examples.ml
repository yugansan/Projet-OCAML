(*some examples of voronoi*)
type seed = {c: color option; x: int; y: int}
type voronoi = {dim: int * int; seeds: seed array}

let v1 = {
  dim = 200,200;
  seeds = [|
    {c=Some red; x=50; y=100};
    {c=Some green; x=100; y=50};
    {c=Some blue; x=100; y=150};
    {c=None; x=150; y=100};
    {c=None; x=100; y=100} |] };;

let v2 = {
  dim = 600,600;
  seeds = [|
    {c = None; x=100; y=100};
    {c = Some red; x=125; y=550};
    {c = None; x=250; y=50};
    {c = Some blue; x=150; y=250};
    {c = None; x=250; y=300};
    {c = None; x=300; y=500};
    {c = Some red; x=400; y=100};
    {c = None; x=450; y=450};
    {c = None; x=500; y=250};
    {c = Some yellow; x=575; y=350};
    {c = Some green; x=300; y=300};
    {c = None; x=75; y=470};
  |]};;

let v3 = {
  dim = 600,600;
  seeds = [|
    {c = None; x=100; y=100};
    {c = Some red; x=125; y=550};
    {c = None; x=250; y=50};
    {c = Some blue; x=150; y=250};
    {c = None; x=250; y=300};
    {c = None; x=300; y=500};
    {c = Some red; x=400; y=100};
    {c = None; x=450; y=450};
    {c = None; x=500; y=250};
    {c = None; x=575; y=350};
    {c = Some green; x=300; y=300};
    {c = None; x=75; y=470};
    {c = None; x=10; y=14};
    {c = Some red; x=122; y=55};
    {c = None; x=25; y=345};
    {c = Some blue; x=23; y=550};
    {c = None; x=25; y=30};
    {c = None; x=367; y=530};
    {c = None; x=434; y=10};
    {c = None; x=45; y=50};
    {c = None; x=50; y=25};
    {c = Some yellow; x=578; y=550};
    {c = Some green; x=30; y=350};
    {c = None; x=375; y=47};
  |]};;


let v4 =  {
    dim = 800,800;
    seeds = [|
              {c = None; x=100; y=75};
              {c = None; x=125; y=225};
              {c = Some red; x=25; y=255};
              {c = None; x=60; y=305};
              {c = Some blue; x=50; y=400};
              {c = Some green; x=100; y=550};
              {c = Some green; x=150; y=25};
              {c = Some red; x=200; y=55};
              {c = None; x=200; y=200};
              {c = None; x=250; y=300};
              {c = None; x=300; y=450};
              {c = None; x=350; y=10};
              {c = None; x=357; y=75};
              {c = Some yellow; x=450; y=80};
              {c = Some blue; x=400; y=150};
              {c = None; x=550; y=350};
              {c = None; x=400; y=450};
              {c = None; x=400; y=500};
              {c = Some red; x=500; y=75};
              {c = Some green; x=600; y=100};
              {c = Some red; x=700; y=75};
              {c = None; x=578; y=175};
              {c = None; x=750; y=205};
              {c = None; x=520; y=345};
              {c = None; x=678; y=420};
              {c = None; x=600; y=480};
              {c = Some blue; x=650; y=480};
              {c = None; x=750; y=500};
              {c = None; x=600; y=550};
              {c = Some red; x=700; y=550};
            |]
    };;

(*ADD other voronoi of 30/40 seeds, for example taken from the web-site*)


