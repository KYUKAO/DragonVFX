using System;
using UnityEngine;

public class SceneTransformController : MonoBehaviour
{
    public Transform[] objectsToRotate; // 需要旋转的物体数组
    public Vector3 rotationCenter; // 旋转中心
    public Vector3 rotationAngles; // 应用的旋转角度

    private void Start()
    {
        RotateObjects();
    }

    public void RotateObjects()
    {
        // 创建一个新的空对象作为所有物体的临时父对象
        GameObject temporaryParent = new GameObject("TempParent");
        temporaryParent.transform.position = rotationCenter;

        // 将所有物体移到临时父对象下
        foreach (var obj in objectsToRotate)
        {
            obj.SetParent(temporaryParent.transform, true);
        }

        // 应用旋转
        temporaryParent.transform.rotation = Quaternion.Euler(rotationAngles);

        // 将所有物体移回它们原来的父对象
        foreach (Transform obj in temporaryParent.transform)
        {
            obj.SetParent(null, true);
        }

        // 删除临时父对象
        Destroy(temporaryParent);
    }
}