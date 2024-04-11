Shader "CustomVignetteURP"
{
    Properties
    {
        _MainTex("Texture", 2D) = "white" {}
        _VignetteIntensity("Vignette Intensity", Range(0, 3)) = 1
        _VignetteSmoothness("Vignette Smoothness", Range(0, 5)) = 1
    }
    SubShader
    {
        Pass
        {
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #include <UnityShaderUtilities.cginc>

            // #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"

            struct appdata
            {
                float4 vertex : POSITION;
                float2 uv : TEXCOORD0;
            };

            struct v2f
            {
                float2 uv : TEXCOORD0;
                float4 vertex : SV_POSITION;
            };

            sampler2D _MainTex;
            float4 _MainTex_TexelSize;
            float _VignetteIntensity;
            float _VignetteSmoothness;

            v2f vert (appdata v)
            {
                v2f o;
                o.vertex = UnityObjectToClipPos(v.vertex);
                o.uv = v.uv;
                return o;
            }

            half4 frag (v2f i) : SV_Target
            {
                half4 col = tex2D(_MainTex, i.uv);

                // Vignette effect
                float2 screenUV = abs(i.uv - float2(0.5f,0.5f)) * _VignetteIntensity;
                screenUV = pow(saturate(screenUV), _VignetteSmoothness);
                float dist = length(screenUV);
                float vfactor = pow(saturate(1 - dist * dist), _VignetteSmoothness);
                col.rgb *= vfactor;

                return col;
            }
            ENDHLSL
        }
    }
}
