using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class VignetteControl : MonoBehaviour
{
    [Range(0, 3)]
    public float vignetteIntensity = 1.8f;
    [Range(0, 5)]
    public float vignetteSmoothness = 5;
    private Material vignetteMat;
    // Start is called before the first frame update
    void Start()
    {

    }

    // Update is called once per frame
    void Update()
    {

    }
    private void OnRenderImage(RenderTexture source, RenderTexture destination)
    {
        if (vignetteMat == null)
        {
            vignetteMat = new Material(Shader.Find("CustomVignetteURP"));
            Debug.Log("1");
        }
        if (vignetteMat == null || vignetteMat.shader == null || vignetteMat.shader.isSupported == false)
        {
            Debug.Log("2");
            return;
        }
        vignetteMat.SetFloat("_VignetteIntensity", vignetteIntensity);
        vignetteMat.SetFloat("_VignetteSmoothness", vignetteSmoothness);
        Graphics.Blit(source, destination, vignetteMat, 0);
    }
}