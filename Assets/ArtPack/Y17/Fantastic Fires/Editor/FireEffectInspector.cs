using UnityEngine;
using UnityEditor;

public class FireEffectInspector : MaterialEditor
{
	public override void OnInspectorGUI()
	{
		base.OnInspectorGUI();
		EditorGUILayout.HelpBox("Use the FireEffect component to control temperature, opacity, tiling and animation speed!", MessageType.Info);
	}
}