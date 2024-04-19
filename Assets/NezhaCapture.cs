using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEditor;

public class NezhaCapture : MonoBehaviour
{
    public CameraCapture cameraCapture;
    public GameObject parentObject;
    private int photoNum = 0;
    
    private void Start()
    {

        // 遍历所有子物体
        foreach (Transform child in parentObject.transform)
        {
            child.gameObject.SetActive(true); // 激活子物体
            cameraCapture.fileName = photoNum.ToString()+".png";
            StartCoroutine(Capture());
            child.gameObject.SetActive(false); // 激活子物体
            photoNum++;
        }
    }
    IEnumerator Capture()
    {
        cameraCapture.saveCapture(); // 调用CameraCapture的Save函数
        yield return new WaitForSeconds(4f);
    }
}
