using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.Rendering.Universal;
public class VignetteControl : ScriptableRendererFeature//脚本原打算用于后处理暗角效果，尚未使用
{
    class VignettePass : ScriptableRenderPass
    {
        private Material vignetteMaterial;
        private RenderTargetIdentifier source;
        private RenderTargetHandle temporaryRenderTargetHandle;

        public float vignetteIntensity;
        public float vignetteSmoothness;

        public VignettePass(Material material)
        {
            vignetteMaterial = material;
            temporaryRenderTargetHandle.Init("_TemporaryColorTexture");
        }

        public void Setup(RenderTargetIdentifier sourceIdentifier, float intensity, float smoothness)
        {
            this.source = sourceIdentifier;
            this.vignetteIntensity = intensity;
            this.vignetteSmoothness = smoothness;
        }

        public override void Execute(ScriptableRenderContext context, ref RenderingData renderingData)
        {
            vignetteMaterial.SetFloat("_VignetteIntensity", vignetteIntensity);
            vignetteMaterial.SetFloat("_VignetteSmoothness", vignetteSmoothness);

            CommandBuffer cmd = CommandBufferPool.Get("Vignette Effect");
            RenderTextureDescriptor opaqueDesc = renderingData.cameraData.cameraTargetDescriptor;
            opaqueDesc.depthBufferBits = 0;

            cmd.GetTemporaryRT(temporaryRenderTargetHandle.id, opaqueDesc, FilterMode.Bilinear);
            Blit(cmd, source, temporaryRenderTargetHandle.Identifier(), vignetteMaterial, 0);
            Blit(cmd, temporaryRenderTargetHandle.Identifier(), source);

            context.ExecuteCommandBuffer(cmd);
            CommandBufferPool.Release(cmd);
        }
    }

    [SerializeField]
    private Shader vignetteShader;
    private Material vignetteMaterial;
    private VignettePass vignettePass;

    public float vignetteIntensity = 1.8f;
    public float vignetteSmoothness = 5;

    public override void Create()
    {
        vignetteMaterial = CoreUtils.CreateEngineMaterial(vignetteShader);
        vignettePass = new VignettePass(vignetteMaterial)
        {
            renderPassEvent = RenderPassEvent.AfterRenderingTransparents
        };
    }

    public override void AddRenderPasses(ScriptableRenderer renderer, ref RenderingData renderingData)
    {
        vignettePass.Setup(renderer.cameraColorTarget, vignetteIntensity, vignetteSmoothness);
        renderer.EnqueuePass(vignettePass);
    }
}
