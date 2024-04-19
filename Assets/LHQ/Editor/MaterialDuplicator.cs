using UnityEngine;
using UnityEditor;

public class MaterialDuplicator : EditorWindow
{
    private int startN;
    private int endK;
    private GameObject targetGameObject;

    [MenuItem("Tools/Material Duplicator")]
    public static void ShowWindow()
    {
        GetWindow(typeof(MaterialDuplicator));
    }

    void OnGUI()
    {
        GUILayout.Label("Base Settings", EditorStyles.boldLabel);
        startN = EditorGUILayout.IntField("Start N", startN);
        endK = EditorGUILayout.IntField("End K", endK);
        targetGameObject = EditorGUILayout.ObjectField("Target GameObject", targetGameObject, typeof(GameObject), true) as GameObject;

        if (GUILayout.Button("Duplicate and Apply Materials"))
        {
            if (targetGameObject == null)
            {
                Debug.LogError("No target GameObject selected.");
                return;
            }
            DuplicateMaterials(startN, endK, targetGameObject);
        }
    }

    private static void DuplicateMaterials(int n, int k, GameObject targetGameObject)
    {
        Material originalMaterial = Selection.activeObject as Material;
        if (originalMaterial == null)
        {
            Debug.LogError("No material selected.");
            return;
        }

        for (int i = n; i <= k; i++)
        {
            // 复制材质并设置新名称
            Material newMaterial = new Material(originalMaterial);
            newMaterial.name = "M_" + i;

            // 加载并设置纹理
            string texturePath = $"Assets/{i}.png";
            Texture2D texture = AssetDatabase.LoadAssetAtPath<Texture2D>(texturePath);
            if (texture != null)
            {
                newMaterial.SetTexture("_BaseMap", texture);
            }
            else
            {
                Debug.LogWarning($"Texture not found: {texturePath}");
            }

            // 保存新材质到Assets文件夹
            AssetDatabase.CreateAsset(newMaterial, $"Assets/M_{i}.mat");

            // 应用新材质到指定的子GameObject
            if (targetGameObject.transform.childCount > i)
            {
                GameObject child = targetGameObject.transform.GetChild(i).gameObject;
                Renderer renderer = child.GetComponent<Renderer>();
                if (renderer != null)
                {
                    renderer.material = newMaterial;
                }
                else
                {
                    Debug.LogWarning($"No Renderer found on {child.name}");
                }
            }
            else
            {
                Debug.LogWarning($"No child at index {i} in {targetGameObject.name}");
            }
        }

        AssetDatabase.SaveAssets();
        AssetDatabase.Refresh();
    }
}
