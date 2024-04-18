using UnityEditor;

using UnityEngine;



/// <summary>

/// �����ͼ �༭��

/// <para>ZhangYu 2018-07-06</para>

/// </summary>

[CanEditMultipleObjects]

[CustomEditor(typeof(CameraCapture))]

public class CameraCaptureEditor : Editor
{



    public override void OnInspectorGUI()
    {

        // ����

        CameraCapture script = (CameraCapture)target;

        int selected = (int)script.captureSize;



        // �ػ�GUI

        EditorGUI.BeginChangeCheck();

        drawProperty("targetCamera", "Ŀ�����");

        string[] options = new string[] { "����ߴ�", "��Ļ�ߴ�", "�̶��ߴ�" };

        selected = EditorGUILayout.Popup("��ͼ�ߴ�", selected, options, GUILayout.ExpandWidth(true));

        script.captureSize = (CameraCapture.CaptureSize)selected;

        if (script.captureSize == CameraCapture.CaptureSize.FixedSize)
        {

            drawProperty("pixelSize", "���سߴ�");

            EditorGUILayout.HelpBox("�뱣����ȷ�Ŀ�߱ȣ�\n�����ͼ������ܳ��ִ���", MessageType.Info);

        }

        drawProperty("savePath", "����·��");

        drawProperty("fileName", "�ļ�����");



        // �����ͼ��ť

        bool isPress = GUILayout.Button("�����ͼ", GUILayout.ExpandWidth(true));

        if (isPress) script.saveCapture();

        if (EditorGUI.EndChangeCheck()) serializedObject.ApplyModifiedProperties();

    }



    private void drawProperty(string property, string label)
    {

        EditorGUILayout.PropertyField(serializedObject.FindProperty(property), new GUIContent(label), true);

    }



}