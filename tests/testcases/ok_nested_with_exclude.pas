{ Testcase of classes nested in classes. This is a reworked version of code
  from Castle Game Engine. We use _exclude on some nested items,
  which requires proper TPasDoc.RemoveExcludedItems implementation. }
unit ok_nested_with_exclude;

interface

type
  { Renderer capabilities for @code(InternalFeatures). @exclude }
  TGlobalInternalRendererFeatures = record
    Shaders: Boolean;
    MaxVertexUniformComponents: Cardinal;
    MaxSkinJointsInUniforms: Cardinal;
    MaxTextureSize: Cardinal;
    TextureFloat: Boolean;
  end;

  { How do we calculate mesh from joints, for @code(InternalMeshCalculation). @exclude }
  TGlobalInternalMeshCalculation = (
    { Calculate resulting mesh on CPU, using Pascal code. }
    mcCpu,
    { Calculate resulting mesh on GPU, using shaders, with joints
      information passed in uniforms. }
    mcGpuUniforms,
    { Calculate resulting mesh on GPU, using shaders, with joints
      information passed in a texture. }
    mcGpuTexture
  );

  TSkinNode = class(TX3DNode)
  public
    type
      { Renderer capabilities for @code(InternalFeatures). @exclude }
      TInternalRendererFeatures = record
        Shaders: Boolean;
        MaxVertexUniformComponents: Cardinal;
        MaxSkinJointsInUniforms: Cardinal;
        MaxTextureSize: Cardinal;
        TextureFloat: Boolean;
      end;

      { How do we calculate mesh from joints, for @code(InternalMeshCalculation). @exclude }
      TInternalMeshCalculation = (
        { Calculate resulting mesh on CPU, using Pascal code. }
        mcCpu,
        { Calculate resulting mesh on GPU, using shaders, with joints
          information passed in uniforms. }
        mcGpuUniforms,
        { Calculate resulting mesh on GPU, using shaders, with joints
          information passed in a texture. }
        mcGpuTexture
      );

    var
      { Current joint matrix.
        See https://www.khronos.org/files/gltf20-reference-guide.pdf
        for explanation.
        Having this ready, and of proper size, speeds up InternalUpdateSkin.
        Moreover, it is passed to shaders when we do skinning on GPU,
        so it has to be accessible during rendering.
        @exclude }
      InternalJointMatrix: TMatrix4List;

      { If @code(InternalMeshCalculation) is mcGpuTexture,
        and once it is initialized (detect by FdJoints.Count = InternalJointMatrix.Count),
        use this texture to pass joint matrices to shaders.

        It has defined contents (and may be @nil) otherwise.
        @exclude }
      InternalJointTexture: TPixelTextureNode;

    constructor Create(const AX3DName, ABaseUrl: String); override;
    destructor Destroy; override;

    { Update meshes in @link(FdShapes) to reflect the current joints transformation.
      This does the core of "skinned animation" logic.
      @exclude }
    procedure InternalUpdateSkin;

    { How is mesh updated from joints, in particular do we use shaders.
      This takes into account InternalFeatures (is current rendering
      context capable enough) but also some shape-specific conditions
      (e.g. too many joints, or being a "shadow volumes caster", prevent
      calculating skin on GPU).

      @exclude }
    function InternalMeshCalculation: TInternalMeshCalculation;

  end;

implementation
end.