using UnityEditor;

using UnityEngine;



/// <summary>

/// 相机截图 编辑器

/// <para>ZhangYu 2018-07-06</para>

/// </summary>

[CanEditMultipleObjects]

[CustomEditor(typeof(CameraCapture))]

public class CameraCaptureEditor : Editor
{



    public override void OnInspectorGUI()
    {

        // 属性

        CameraCapture script = (CameraCapture)target;

        int selected = (int)script.captureSize;



        // 重绘GUI

        EditorGUI.BeginChangeCheck();

        drawProperty("targetCamera", "目标像机");

        string[] options = new string[] { "像机尺寸", "屏幕尺寸", "固定尺寸" };

        selected = EditorGUILayout.Popup("截图尺寸", selected, options, GUILayout.ExpandWidth(true));

        script.captureSize = (CameraCapture.CaptureSize)selected;

        if (script.captureSize == CameraCapture.CaptureSize.FixedSize)
        {

            drawProperty("pixelSize", "像素尺寸");

            EditorGUILayout.HelpBox("请保持正确的宽高比！\n否则截图区域可能出现错误。", MessageType.Info);

        }

        drawProperty("savePath", "保存路径");

        drawProperty("fileName", "文件名称");



        // 保存截图按钮

        bool isPress = GUILayout.Button("保存截图", GUILayout.ExpandWidth(true));

        if (isPress) script.saveCapture();

        if (EditorGUI.EndChangeCheck()) serializedObject.ApplyModifiedProperties();

    }



    private void drawProperty(string property, string label)
    {

        EditorGUILayout.PropertyField(serializedObject.FindProperty(property), new GUIContent(label), true);

    }



}