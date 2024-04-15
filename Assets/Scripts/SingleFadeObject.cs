using System.Collections;
using UnityEngine;
using UnityEngine.Serialization;

public class SingleFadeObject : MonoBehaviour
{
    public AnimationCurve FadeCurve; // 定义透明度随时间变化的曲线
    public float FadeInTime = 1f; // 淡入时间
    public float FadeOutTime = 1f; // 淡出时间
    public float FadeMin;
    public float FadeMax;
    public float StayTransparentTime = 2f; // 控制在透明度为1时保持的时间
    public string ColorPropertyName = "_Color"; // 材质颜色属性名
    public int Queue = 3000;

    private Renderer[] renderers;

    private void Start()
    {
        renderers = GetComponentsInChildren<Renderer>();
        StartCoroutine(FadeSequence());
    }

    IEnumerator FadeSequence()
    {
        yield return StartCoroutine(Fade(FadeMin, FadeMax, FadeInTime)); // 淡入

        float startTime = Time.time;

        while (Time.time < startTime + StayTransparentTime)
        {
            yield return null; // 逐帧更新，保持透明
        }

        yield return StartCoroutine(Fade(FadeMax, FadeMin, FadeOutTime)); // 淡出

        // 淡出完成后，禁用物体
        gameObject.SetActive(false);
    }

    IEnumerator Fade(float startAlpha, float endAlpha, float duration)
    {
        float elapsedTime = 0f;
        Color color = Color.clear;

        while (elapsedTime < duration)
        {
            float newAlpha = Mathf.Lerp(startAlpha, endAlpha, FadeCurve.Evaluate(elapsedTime / duration));

            foreach (Renderer renderer in renderers)
            {
                foreach (Material material in renderer.materials)
                {
                    material.renderQueue = Queue;
                    material.SetFloat(ColorPropertyName, newAlpha);
                }
            }

            elapsedTime += Time.deltaTime;
            yield return null;
        }

        // 确保最终透明度为endAlpha
        foreach (Renderer renderer in renderers)
        {
            foreach (Material material in renderer.materials)
            {
                material.renderQueue = Queue;
                material.SetFloat(ColorPropertyName, endAlpha);
            }
        }
    }
}
