using UnityEngine;
using UnityEditor;

public class AdjustMeshImportSettings : ScriptableObject
{
    [MenuItem("Tools/Adjust Mesh Import Settings For Selected and Their Children")]
    static void AdjustSettings()
    {
        foreach (GameObject obj in Selection.gameObjects)
        {
            // Adjust settings for the selected object
            AdjustSettingsForObject(obj);

            // Recursively adjust settings for all children
            foreach (Transform child in obj.transform)
            {
                AdjustSettingsRecursively(child);
            }
        }
        
        AssetDatabase.Refresh();
    }

    static void AdjustSettingsRecursively(Transform transform)
    {
        // Adjust settings for the current child
        AdjustSettingsForObject(transform.gameObject);

        // Recursively adjust settings for all children
        foreach (Transform child in transform)
        {
            AdjustSettingsRecursively(child);
        }
    }

    static void AdjustSettingsForObject(GameObject obj)
    {
        MeshFilter meshFilter = obj.GetComponent<MeshFilter>();
        if (meshFilter != null && meshFilter.sharedMesh != null)
        {
            string assetPath = AssetDatabase.GetAssetPath(meshFilter.sharedMesh);
            if (!string.IsNullOrEmpty(assetPath))
            {
                ModelImporter importer = AssetImporter.GetAtPath(assetPath) as ModelImporter;
                if (importer != null)
                {
                    importer.materialImportMode = ModelImporterMaterialImportMode.ImportStandard;
                    importer.materialLocation = ModelImporterMaterialLocation.External;
                    importer.materialName = ModelImporterMaterialName.BasedOnMaterialName;
                    importer.SaveAndReimport();
                }
            }
        }
    }
}