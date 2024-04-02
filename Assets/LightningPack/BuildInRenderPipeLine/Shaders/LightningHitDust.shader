// Shader created with Shader Forge v1.38 
// Shader Forge (c) Freya Holmer - http://www.acegikmo.com/shaderforge/
// Note: Manually altering this data may prevent you from opening it in Shader Forge
/*SF_DATA;ver:1.38;sub:START;pass:START;ps:flbk:,iptp:0,cusa:False,bamd:0,cgin:,lico:0,lgpr:1,limd:0,spmd:0,trmd:0,grmd:0,uamb:True,mssp:True,bkdf:False,hqlp:False,rprd:False,enco:False,rmgx:True,imps:True,rpth:0,vtps:0,hqsc:True,nrmq:0,nrsp:0,vomd:0,spxs:False,tesm:0,olmd:1,culm:2,bsrc:3,bdst:7,dpts:2,wrdp:False,dith:0,atcv:False,rfrpo:True,rfrpn:Refraction,coma:15,ufog:False,aust:False,igpj:True,qofs:0,qpre:3,rntp:2,fgom:False,fgoc:True,fgod:False,fgor:False,fgmd:0,fgcr:0,fgcg:0,fgcb:0,fgca:1,fgde:0.01,fgrn:0,fgrf:300,stcl:False,atwp:False,stva:128,stmr:255,stmw:255,stcp:6,stps:0,stfa:0,stfz:0,ofsf:0,ofsu:0,f2p0:False,fnsp:True,fnfb:True,fsmp:False;n:type:ShaderForge.SFN_Final,id:4795,x:34080,y:34530,varname:node_4795,prsc:2|emission-4525-OUT,alpha-2371-A;n:type:ShaderForge.SFN_Blend,id:6586,x:33512,y:34732,varname:node_6586,prsc:2,blmd:1,clmp:False|SRC-2371-RGB,DST-9383-RGB;n:type:ShaderForge.SFN_Color,id:9383,x:33213,y:34745,ptovrint:False,ptlb:color,ptin:_color,varname:node_9383,prsc:2,glob:False,taghide:False,taghdr:True,tagprd:False,tagnsco:False,tagnrm:False,c1:0.990566,c2:0.990566,c3:0.990566,c4:1;n:type:ShaderForge.SFN_ValueProperty,id:7492,x:33443,y:34338,ptovrint:False,ptlb:Power,ptin:_Power,varname:node_7492,prsc:2,glob:False,taghide:False,taghdr:False,tagprd:False,tagnsco:False,tagnrm:False,v1:3;n:type:ShaderForge.SFN_Multiply,id:4525,x:33751,y:34507,varname:node_4525,prsc:2|A-7492-OUT,B-6586-OUT;n:type:ShaderForge.SFN_Tex2d,id:2371,x:33251,y:35057,ptovrint:False,ptlb:MainTexture,ptin:_MainTexture,varname:node_2371,prsc:2,glob:False,taghide:False,taghdr:False,tagprd:False,tagnsco:False,tagnrm:False,ntxv:0,isnm:False;proporder:9383-7492-2371;pass:END;sub:END;*/

Shader "Todonero/LightningDust" {
    Properties {
        [HDR]_color ("color", Color) = (0.990566,0.990566,0.990566,1)
        _Power ("Power", Float ) = 3
        _MainTexture ("MainTexture", 2D) = "white" {}
        [HideInInspector]_Cutoff ("Alpha cutoff", Range(0,1)) = 0.5
    }
    SubShader {
        Tags {
            "IgnoreProjector"="True"
            "Queue"="Transparent"
            "RenderType"="Transparent"
        }
        Pass {
            Name "FORWARD"
            Tags {
                "LightMode"="ForwardBase"
            }
            Blend SrcAlpha OneMinusSrcAlpha
            Cull Off
            ZWrite Off
            
            CGPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #include "UnityCG.cginc"
            #pragma multi_compile_fwdbase
            #pragma only_renderers d3d9 d3d11 glcore gles gles3 metal d3d11_9x xboxone ps4 psp2 n3ds wiiu switch 
            #pragma target 3.0
            uniform float4 _color;
            uniform float _Power;
            uniform sampler2D _MainTexture; uniform float4 _MainTexture_ST;
            struct VertexInput {
                float4 vertex : POSITION;
                float2 texcoord0 : TEXCOORD0;
            };
            struct VertexOutput {
                float4 pos : SV_POSITION;
                float2 uv0 : TEXCOORD0;
            };
            VertexOutput vert (VertexInput v) {
                VertexOutput o = (VertexOutput)0;
                o.uv0 = v.texcoord0;
                o.pos = UnityObjectToClipPos( v.vertex );
                return o;
            }
            float4 frag(VertexOutput i, float facing : VFACE) : COLOR {
                float isFrontFace = ( facing >= 0 ? 1 : 0 );
                float faceSign = ( facing >= 0 ? 1 : -1 );
////// Lighting:
////// Emissive:
                float4 _MainTexture_var = tex2D(_MainTexture,TRANSFORM_TEX(i.uv0, _MainTexture));
                float3 emissive = (_Power*(_MainTexture_var.rgb*_color.rgb));
                float3 finalColor = emissive;
                return fixed4(finalColor,_MainTexture_var.a);
            }
            ENDCG
        }
    }
    CustomEditor "ShaderForgeMaterialInspector"
}
