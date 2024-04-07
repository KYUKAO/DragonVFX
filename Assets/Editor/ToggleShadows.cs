using UnityEngine;
using UnityEditor;

public class ToggleShadows : EditorWindow
{
    [MenuItem("Tools/Toggle Shadows for Selected Objects")]
    static void Toggle()
    {
        GameObject[] selectedObjects = Selection.gameObjects;

        foreach (GameObject obj in selectedObjects)
        {
            ToggleShadowsRecursive(obj.transform, 0);
        }
    }

    static void ToggleShadowsRecursive(Transform parent, int depth)
    {
        if (depth >= 3) return; // Stop recursion at third level

        foreach (Transform child in parent)
        {
            MeshRenderer renderer = child.GetComponent<MeshRenderer>();
            if (renderer != null)
            {
                renderer.shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off;
            }

            ToggleShadowsRecursive(child, depth + 1);
        }
    }
}