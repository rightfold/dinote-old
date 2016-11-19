module NN.Interpret
( interpret
, runNNDSL
, runVertexDSL
) where

import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Eff.Ref (readRef, REF, Ref, writeRef)
import Data.Map (Map)
import Data.Map as Map
import NN.DSL (NNDSL)
import NN.Prelude
import NN.Vertex (Vertex, VertexID(..))
import NN.Vertex.DSL (VertexDSL, VertexDSLF(..))
import NN.Vertex.Note (Note(..))
import NN.Vertex.Style (Style(..))

interpret
    :: ∀ eff
     . Ref (Map VertexID (BusRW Vertex))
    -> Free (Aff (avar :: AVAR, ref :: REF | eff) ⊕ NNDSL)
    ~> Aff (avar :: AVAR, ref :: REF | eff)
interpret busesRef = foldFree (coproduct id (runNNDSL busesRef))

runNNDSL
    :: ∀ eff
     . Ref (Map VertexID (BusRW Vertex))
    -> NNDSL
    ~> Aff (avar :: AVAR, ref :: REF | eff)
runNNDSL = runVertexDSL

runVertexDSL
    :: ∀ eff
     . Ref (Map VertexID (BusRW Vertex))
    -> VertexDSL
    ~> Aff (avar :: AVAR, ref :: REF | eff)
runVertexDSL busesRef = foldFree go
    where
    go :: VertexDSLF ~> Aff (avar :: AVAR, ref :: REF | eff)
    go (GetVertex vertexID a) = pure $ a $ case vertexID of
        VertexID "92eacb4c-a841-4b96-a984-a077caba347c" -> Just {note: Text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris placerat diam sollicitudin leo vehicula scelerisque. Vivamus et pulvinar purus. Aenean sagittis, leo eu tempus accumsan, tellus ex tempor nisi, vel lobortis dolor neque ornare libero. Nulla facilisi. Mauris fermentum interdum pretium. Pellentesque a erat vel ipsum hendrerit ullamcorper. Sed tempus, mi nec scelerisque imperdiet, neque dui sagittis lorem, sed sodales augue ante eget lacus.", children: VertexID "9733d16e-d506-428a-a135-c3e7d886c396" : VertexID "78c48c7e-7fad-48a8-815f-9f4c2ce43fd7" : Nil, style: Normal}
        VertexID "9733d16e-d506-428a-a135-c3e7d886c396" -> Just {note: Text "Cras posuere augue eros, ut viverra eros rhoncus molestie. Sed ut sodales nisl. Suspendisse nec arcu non arcu fermentum ornare ac non est. Praesent sit amet leo eu tellus interdum ullamcorper. Sed odio neque, vulputate eget accumsan at, cursus non mauris. Maecenas euismod consectetur ante. In hac habitasse platea dictumst. Cras euismod dolor nec lectus facilisis lobortis. Praesent efficitur faucibus velit, maximus viverra diam molestie a. Mauris ut felis vel nisl accumsan ornare in non est. Cras congue libero sapien. Sed in accumsan sem, tempus finibus tortor. Curabitur dapibus iaculis viverra. Donec sagittis cursus libero a porta. Sed est mauris, tincidunt nec porttitor nec, rutrum non elit.", children: VertexID "d2e7f648-e8a7-4891-8754-4543498a8f57" : Nil, style: Dimmed}
        VertexID "78c48c7e-7fad-48a8-815f-9f4c2ce43fd7" -> Just {note: Text "Ut feugiat augue nisl, sit amet posuere metus suscipit quis. Nullam fringilla ut enim ac scelerisque. Integer in lobortis quam. Vivamus turpis libero, hendrerit vitae sem vitae, faucibus hendrerit nunc. Phasellus ut mi mattis, facilisis ipsum quis, tempus lorem. Nulla eu ipsum a sem congue ultrices. Donec lobortis dapibus lorem et pellentesque.", children: VertexID "d2e7f648-e8a7-4891-8754-4543498a8f57" : VertexID "92eacb4c-a841-4b96-a984-a077caba347c" : Nil, style: Peachpuff}
        VertexID "d2e7f648-e8a7-4891-8754-4543498a8f57" -> Just {note: Text "Morbi dolor orci, gravida vitae porttitor nec, ornare sed urna. Maecenas finibus fringilla pretium. Suspendisse aliquet, orci egestas molestie vestibulum, velit erat faucibus risus, id suscipit nisi magna vitae elit. Aliquam consectetur velit quis massa semper, sodales feugiat dui tristique. In consectetur erat vitae odio volutpat, imperdiet interdum ipsum sodales. Nulla rhoncus mauris sed ullamcorper blandit. Pellentesque ac rutrum eros. Ut mollis tincidunt odio, tempus ultricies massa vulputate sed. Vestibulum nibh ligula, gravida in sem vel, commodo dignissim urna. Integer a mi mattis, blandit nulla sit amet, vestibulum sem. Morbi eleifend maximus massa, vel iaculis sem tristique a. Pellentesque commodo, tellus eget gravida hendrerit, erat dolor tempor ante, et lacinia urna nibh vel odio. Nulla condimentum nisl condimentum mollis interdum. Proin nec viverra quam, sed sollicitudin tellus. Duis facilisis nunc augue, a posuere libero eleifend non. Vivamus lobortis efficitur orci finibus porttitor.", children: VertexID "0fe66fb3-c195-49d1-a31b-983d8191f6a5" : VertexID "5c9151bc-df1d-4ff0-87c4-60e66642b8a9" : Nil, style: HotdogStand}
        VertexID "0fe66fb3-c195-49d1-a31b-983d8191f6a5" -> Just {note: Text "Aenean condimentum enim quis nunc pellentesque, mattis sodales justo interdum. Sed gravida elit fringilla dolor commodo aliquam vel id leo. Integer congue porta orci, tincidunt placerat lectus. Mauris viverra felis id pretium pellentesque. Maecenas quis dolor vitae ante porta tincidunt eu non magna. Vestibulum fringilla placerat sem, vel placerat enim posuere lobortis. Suspendisse a quam mauris. Vestibulum auctor tincidunt lectus, malesuada aliquet turpis cursus a. Nulla pulvinar scelerisque erat, sit amet convallis mi tempor id. Curabitur euismod eleifend neque, ut volutpat magna. Nam vel massa eget erat pulvinar accumsan. Sed semper et erat et viverra. Pellentesque eget ipsum nisi.", children: Nil, style: Grass}
        VertexID "5c9151bc-df1d-4ff0-87c4-60e66642b8a9" -> Just {note: Text "Donec quis rhoncus augue. Vestibulum faucibus, purus a porttitor eleifend, odio purus aliquam urna, id convallis diam metus accumsan ex. Nunc placerat sollicitudin turpis eget molestie. Proin sed eros vel enim accumsan aliquet. Proin ligula ipsum, aliquam in lacinia eu, pulvinar ac tellus. Fusce erat mi, venenatis ultricies aliquet eu, lacinia at quam. Nullam nec enim mi. Quisque id rhoncus nibh, id hendrerit leo.", children: Nil, style: Ocean}
        _ -> Nothing
    go (VertexBus vertexID a) = do
        freshBus <- Bus.make
        bus <- liftEff $ do
            buses <- readRef busesRef
            case Map.lookup vertexID buses of
                Nothing -> do
                    writeRef busesRef $ Map.insert vertexID freshBus buses
                    pure freshBus
                Just bus -> pure bus
        pure $ a bus
