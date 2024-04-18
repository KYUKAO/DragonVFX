using UnityEngine;

using System.IO;



/// <summary>

/// �����ͼ

/// <para>ZhangYu 2018-07-06</para>

/// </summary>

public class CameraCapture : MonoBehaviour
{

    // ��ͼ�ߴ�
    public enum CaptureSize
    {

        CameraSize,

        ScreenResolution,

        FixedSize

    }



    // Ŀ�������

    public Camera targetCamera;

    // ��ͼ�ߴ�

    public CaptureSize captureSize = CaptureSize.CameraSize;

    // ���سߴ�

    public Vector2 pixelSize;

    // ����·��

    public string savePath = "StreamingAssets/";

    // �ļ�����

    public string fileName = "cameraCapture.png";



#if UNITY_EDITOR

    private void Reset()
    {

        targetCamera = GetComponent<Camera>();

        pixelSize = new Vector2(Screen.currentResolution.width, Screen.currentResolution.height);

    }

#endif



    /// <summary> �����ͼ </summary>

    /// <param name="camera">Ŀ�������</param>

    public void saveCapture()
    {

        Vector2 size = pixelSize;

        if (captureSize == CaptureSize.CameraSize)
        {

            size = new Vector2(targetCamera.pixelWidth, targetCamera.pixelHeight);

        }
        else if (captureSize == CaptureSize.ScreenResolution)
        {

            size = new Vector2(Screen.currentResolution.width, Screen.currentResolution.height);

        }

        string path = Application.dataPath + "/" + savePath + fileName;

        saveTexture(path, capture(targetCamera, (int)size.x, (int)size.y));

    }



    /// <summary> �����ͼ </summary>

    /// <param name="camera">Ŀ�����</param>

    public static Texture2D capture(Camera camera)
    {

        return capture(camera, Screen.width, Screen.height);

    }



    /// <summary> �����ͼ </summary>

    /// <param name="camera">Ŀ�����</param>

    /// <param name="width">���</param>

    /// <param name="height">�߶�</param>

    public static Texture2D capture(Camera camera, int width, int height)
    {

        RenderTexture rt = new RenderTexture(width, height, 0);

        rt.depth = 24;

        rt.antiAliasing = 8;

        camera.targetTexture = rt;

        camera.RenderDontRestore();

        RenderTexture.active = rt;

        Texture2D texture = new Texture2D(width, height, TextureFormat.ARGB32, false, true);

        Rect rect = new Rect(0, 0, width, height);

        texture.ReadPixels(rect, 0, 0);

        texture.filterMode = FilterMode.Point;

        texture.Apply();

        camera.targetTexture = null;

        RenderTexture.active = null;

        Destroy(rt);

        return texture;

    }



    /// <summary> ������ͼ </summary>

    /// <param name="path">����·��</param>

    /// <param name="texture">Texture2D</param>

    public static void saveTexture(string path, Texture2D texture)
    {

        File.WriteAllBytes(path, texture.EncodeToPNG());

#if UNITY_EDITOR

        Debug.Log("�ѱ����ͼ��:" + path);

#endif

    }

}
