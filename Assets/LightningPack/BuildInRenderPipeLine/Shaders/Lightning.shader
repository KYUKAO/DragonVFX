// Shader created with Shader Forge v1.38 
// Shader Forge (c) Freya Holmer - http://www.acegikmo.com/shaderforge/
// Note: Manually altering this data may prevent you from opening it in Shader Forge
/*SF_DATA;ver:1.38;sub:START;pass:START;ps:flbk:,iptp:0,cusa:False,bamd:0,cgin:,lico:0,lgpr:1,limd:0,spmd:0,trmd:0,grmd:0,uamb:True,mssp:True,bkdf:False,hqlp:False,rprd:False,enco:False,rmgx:True,imps:True,rpth:0,vtps:0,hqsc:True,nrmq:0,nrsp:0,vomd:0,spxs:False,tesm:0,olmd:1,culm:2,bsrc:3,bdst:7,dpts:2,wrdp:False,dith:0,atcv:False,rfrpo:True,rfrpn:Refraction,coma:15,ufog:False,aust:False,igpj:True,qofs:0,qpre:3,rntp:2,fgom:False,fgoc:True,fgod:False,fgor:False,fgmd:0,fgcr:0,fgcg:0,fgcb:0,fgca:1,fgde:0.01,fgrn:0,fgrf:300,stcl:False,atwp:False,stva:128,stmr:255,stmw:255,stcp:6,stps:0,stfa:0,stfz:0,ofsf:0,ofsu:0,f2p0:False,fnsp:True,fnfb:True,fsmp:False;n:type:ShaderForge.SFN_Final,id:4795,x:34080,y:34530,varname:node_4795,prsc:2|emission-4525-OUT,alpha-205-OUT;n:type:ShaderForge.SFN_Tex2d,id:4428,x:32274,y:34128,ptovrint:False,ptlb:DissolveTexture,ptin:_DissolveTexture,varname:node_4428,prsc:2,glob:False,taghide:False,taghdr:False,tagprd:False,tagnsco:False,tagnrm:False,tex:cdebd08e910c2264cabe43bbb901c340,ntxv:0,isnm:False|UVIN-2498-OUT;n:type:ShaderForge.SFN_TexCoord,id:8923,x:30453,y:34532,varname:node_8923,prsc:2,uv:0,uaff:True;n:type:ShaderForge.SFN_Multiply,id:8643,x:31300,y:33961,varname:node_8643,prsc:2|A-9821-TSL,B-4914-OUT;n:type:ShaderForge.SFN_Slider,id:6314,x:30706,y:34111,ptovrint:False,ptlb:Dissolve_U_Scroll,ptin:_Dissolve_U_Scroll,varname:_V_Scroll_copy,prsc:2,glob:False,taghide:False,taghdr:False,tagprd:False,tagnsco:False,tagnrm:False,min:0,cur:26.00385,max:100;n:type:ShaderForge.SFN_TexCoord,id:9889,x:31300,y:33715,varname:node_9889,prsc:2,uv:0,uaff:False;n:type:ShaderForge.SFN_Add,id:5225,x:31524,y:33860,varname:node_5225,prsc:2|A-9889-UVOUT,B-8643-OUT;n:type:ShaderForge.SFN_Time,id:9821,x:31025,y:33937,varname:node_9821,prsc:2;n:type:ShaderForge.SFN_Slider,id:2741,x:30706,y:34216,ptovrint:False,ptlb:Dissolve_V_Scroll,ptin:_Dissolve_V_Scroll,varname:_Dissolve_V_Scroll_copy,prsc:2,glob:False,taghide:False,taghdr:False,tagprd:False,tagnsco:False,tagnrm:False,min:-10,cur:0,max:100;n:type:ShaderForge.SFN_Append,id:4914,x:31100,y:34166,varname:node_4914,prsc:2|A-6314-OUT,B-2741-OUT;n:type:ShaderForge.SFN_Panner,id:1913,x:30940,y:34418,varname:node_1913,prsc:2,spu:1,spv:0|UVIN-8923-UVOUT,DIST-865-OUT;n:type:ShaderForge.SFN_Panner,id:7265,x:31118,y:34646,varname:node_7265,prsc:2,spu:1,spv:1|UVIN-1913-UVOUT,DIST-5075-OUT;n:type:ShaderForge.SFN_If,id:2498,x:31965,y:34241,varname:node_2498,prsc:2|A-1134-OUT,B-1341-OUT,GT-5225-OUT,EQ-7265-UVOUT,LT-5225-OUT;n:type:ShaderForge.SFN_ToggleProperty,id:1134,x:31610,y:34095,ptovrint:False,ptlb:UseCustomData,ptin:_UseCustomData,varname:node_1134,prsc:2,glob:False,taghide:False,taghdr:False,tagprd:False,tagnsco:False,tagnrm:False,on:False;n:type:ShaderForge.SFN_Vector1,id:1341,x:31610,y:34160,varname:node_1341,prsc:2,v1:1;n:type:ShaderForge.SFN_TexCoord,id:8250,x:31638,y:34788,varname:node_8250,prsc:2,uv:0,uaff:False;n:type:ShaderForge.SFN_Blend,id:4086,x:32771,y:34466,varname:node_4086,prsc:2,blmd:17,clmp:True|SRC-9200-OUT,DST-3293-OUT;n:type:ShaderForge.SFN_Distance,id:1154,x:32101,y:34971,varname:node_1154,prsc:2|A-6105-OUT,B-8116-OUT;n:type:ShaderForge.SFN_Vector1,id:8116,x:31821,y:34955,varname:node_8116,prsc:2,v1:1;n:type:ShaderForge.SFN_Power,id:3293,x:32292,y:35025,varname:node_3293,prsc:2|VAL-1154-OUT,EXP-6660-OUT;n:type:ShaderForge.SFN_OneMinus,id:8748,x:32992,y:34519,varname:node_8748,prsc:2|IN-4086-OUT;n:type:ShaderForge.SFN_Power,id:205,x:33194,y:34630,varname:node_205,prsc:2|VAL-8748-OUT,EXP-3748-OUT;n:type:ShaderForge.SFN_Slider,id:3748,x:32799,y:34728,ptovrint:False,ptlb:thin,ptin:_thin,varname:node_3748,prsc:2,glob:False,taghide:False,taghdr:False,tagprd:False,tagnsco:False,tagnrm:False,min:1,cur:20,max:200;n:type:ShaderForge.SFN_RemapRange,id:9200,x:32502,y:34407,varname:node_9200,prsc:2,frmn:0,frmx:1,tomn:0.1,tomx:0.8|IN-4428-R;n:type:ShaderForge.SFN_Blend,id:6586,x:33479,y:34732,varname:node_6586,prsc:2,blmd:1,clmp:False|SRC-205-OUT,DST-9383-RGB;n:type:ShaderForge.SFN_Color,id:9383,x:33049,y:34889,ptovrint:False,ptlb:color,ptin:_color,varname:node_9383,prsc:2,glob:False,taghide:False,taghdr:True,tagprd:False,tagnsco:False,tagnrm:False,c1:1,c2:1,c3:1,c4:1;n:type:ShaderForge.SFN_Slider,id:6660,x:31888,y:35190,ptovrint:False,ptlb:Position,ptin:_Position,varname:node_6660,prsc:2,glob:False,taghide:False,taghdr:False,tagprd:False,tagnsco:False,tagnrm:False,min:1,cur:1,max:100;n:type:ShaderForge.SFN_ValueProperty,id:7492,x:33443,y:34338,ptovrint:False,ptlb:Power,ptin:_Power,varname:node_7492,prsc:2,glob:False,taghide:False,taghdr:False,tagprd:False,tagnsco:False,tagnrm:False,v1:3;n:type:ShaderForge.SFN_Multiply,id:4525,x:33800,y:34431,varname:node_4525,prsc:2|A-7492-OUT,B-6586-OUT;n:type:ShaderForge.SFN_SwitchProperty,id:6105,x:31879,y:34809,ptovrint:False,ptlb:90DegreeRotate,ptin:_90DegreeRotate,varname:node_6105,prsc:2,glob:False,taghide:False,taghdr:False,tagprd:False,tagnsco:False,tagnrm:False,on:False|A-8250-U,B-8250-V;n:type:ShaderForge.SFN_Multiply,id:5075,x:30697,y:34698,varname:node_5075,prsc:2|A-8923-W,B-6649-TSL;n:type:ShaderForge.SFN_Time,id:6649,x:30278,y:34879,varname:node_6649,prsc:2;n:type:ShaderForge.SFN_Multiply,id:865,x:30728,y:34383,varname:node_865,prsc:2|A-6649-TSL,B-8923-Z;proporder:4428-6314-2741-1134-3748-9383-6660-7492-6105;pass:END;sub:END;*/

Shader "Todonero/Lightning" {
    Properties {
        _DissolveTexture ("DissolveTexture", 2D) = "white" {}
        _Dissolve_U_Scroll ("Dissolve_U_Scroll", Range(0, 100)) = 26.00385
        _Dissolve_V_Scroll ("Dissolve_V_Scroll", Range(-10, 100)) = 0
        [MaterialToggle] _UseCustomData ("UseCustomData", Float ) = 0
        _thin ("thin", Range(1, 200)) = 20
        [HDR]_color ("color", Color) = (1,1,1,1)
        _Position ("Position", Range(1, 100)) = 1
        _Power ("Power", Float ) = 3
        [MaterialToggle] _90DegreeRotate ("90DegreeRotate", Float ) = 0
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
            uniform sampler2D _DissolveTexture; uniform float4 _DissolveTexture_ST;
            uniform float _Dissolve_U_Scroll;
            uniform float _Dissolve_V_Scroll;
            uniform fixed _UseCustomData;
            uniform float _thin;
            uniform float4 _color;
            uniform float _Position;
            uniform float _Power;
            uniform fixed _90DegreeRotate;
            struct VertexInput {
                float4 vertex : POSITION;
                float4 texcoord0 : TEXCOORD0;
            };
            struct VertexOutput {
                float4 pos : SV_POSITION;
                float4 uv0 : TEXCOORD0;
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
                float node_2498_if_leA = step(_UseCustomData,1.0);
                float node_2498_if_leB = step(1.0,_UseCustomData);
                float4 node_9821 = _Time;
                float2 node_5225 = (i.uv0+(node_9821.r*float2(_Dissolve_U_Scroll,_Dissolve_V_Scroll)));
                float4 node_6649 = _Time;
                float2 node_2498 = lerp((node_2498_if_leA*node_5225)+(node_2498_if_leB*node_5225),((i.uv0+(node_6649.r*i.uv0.b)*float2(1,0))+(i.uv0.a*node_6649.r)*float2(1,1)),node_2498_if_leA*node_2498_if_leB);
                float4 _DissolveTexture_var = tex2D(_DissolveTexture,TRANSFORM_TEX(node_2498, _DissolveTexture));
                float node_205 = pow((1.0 - saturate(abs((_DissolveTexture_var.r*0.7+0.1)-pow(distance(lerp( i.uv0.r, i.uv0.g, _90DegreeRotate ),1.0),_Position)))),_thin);
                float3 emissive = (_Power*(node_205*_color.rgb));
                float3 finalColor = emissive;
                return fixed4(finalColor,node_205);
            }
            ENDCG
        }
    }
    CustomEditor "ShaderForgeMaterialInspector"
}
